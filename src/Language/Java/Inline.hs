-- | = Inline Java quasiquotation
--
-- See the
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell-quasi-quotation GHC manual>
-- for an introduction to quasiquotation. The quasiquoter exported in this
-- module allows embedding arbitrary Java expressions and blocks of statements
-- inside Haskell code. You can call any Java method and define arbitrary inline
-- code using Java syntax. No FFI required.
--
-- Here is the same example as in "Language.Java", but with inline Java calls:
--
-- @
-- {&#45;\# LANGUAGE DataKinds \#&#45;}
-- {&#45;\# LANGUAGE QuasiQuotes \#&#45;}
-- {&#45;\# OPTIONS_GHC -fplugin=Language.Java.Inline.Plugin \#&#45;}
-- module Object where
--
-- import Language.Java as J
-- import Language.Java.Inline
--
-- newtype Object = Object ('J' (''Class' "java.lang.Object"))
-- instance 'Coercible' Object
--
-- clone :: Object -> IO Object
-- clone obj = [java| $obj.clone() |]
--
-- equals :: Object -> Object -> IO Bool
-- equals obj1 obj2 = [java| $obj1.equals($obj2) |]
--
-- ...
-- @

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Java.Inline
  ( java
  , imports
  , loadJavaWrappers
  ) where

import Data.Data
import Data.Generics (everything, mkQ)
import Data.List (isPrefixOf, intercalate, isSuffixOf, nub)
import Data.String (fromString)
import Foreign.JNI (defineClass)
import Language.Java
import Language.Java.Inline.Magic
import qualified Language.Java.Lexer as Java
import qualified Language.Java.Parser as Java
import qualified Language.Java.Syntax as Java
import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH (Q)
import System.IO.Unsafe (unsafePerformIO)

-- Implementation strategy
--
-- We know we'll need to declare a new wrapper (a Java static method), but we
-- don't know the types of the arguments nor the return type. So we first name
-- this method and generate a Haskell call to it at the quasiquotation site.
-- Then, we register a module finalizer that captures the local scope. At the
-- end of the module, when all type checking is done, our finalizer will be run.
-- By this point the types of all the variables in the local scope that was
-- captured are fully determined. So we can analyze these types to determine
-- what the signature of the wrapper should be, in order to declare it.
--
-- The last step is to ask the Java toolchain to produce .class bytecode from
-- our declarations. We embed this bytecode in the binary, adding a reference to
-- it in the static pointer table (SPT). That way at runtime we can enumerate
-- the bytecode blobs registered in the SPT, and load them into the JVM one by
-- one.

-- | Java code quasiquoter. Example:
--
-- @
-- imports "javax.swing.JOptionPane"
--
-- hello :: IO ()
-- hello = do
--     message <- reflect ("Hello World!" :: Text)
--     [java| JOptionPane.showMessageDialog(null, $message) |]
-- @
--
-- A quasiquote is a snippet of Java code. The code is assumed to be a block
-- (sequence of statements) if the first non whitespace character is a @{@
-- (curly brace) character. Otherwise it's parsed as an expression. Variables
-- with an initial @$@ (dollar) sign are allowed. They have a special meaning:
-- they stand for antiqotation variables (think of them as format specifiers in
-- printf format string). An antiquotation variable @$foo@ is well-scoped if
-- there exists a variable with the name @foo@ in the Haskell context of the
-- quasiquote, whose type is 'Coercible' to a Java primitive or reference type.
--
-- __NOTE:__ In GHC 8.0.2 and earlier, due to
-- <https://ghc.haskell.org/trac/ghc/ticket/12778 #12778>, a quasiquote must
-- always return a boxed value (i.e. an object, not void or a primitive type).
-- This limitation may be lifted in the future.
java :: QuasiQuoter
java = QuasiQuoter
    { quoteExp = \txt -> blockOrExpQQ txt
    , quotePat  = error "Language.Java.Inline: quotePat"
    , quoteType = error "Language.Java.Inline: quoteType"
    , quoteDec  = error "Language.Java.Inline: quoteDec"
    }

antis :: Java.Block -> [String]
antis = nub . everything (++) (mkQ [] (\case Java.Name (Java.Ident ('$':av):_) -> [av]; _ -> []))

getValueName :: String -> Q TH.Name
getValueName v =
    TH.lookupValueName v >>= \case
      Nothing -> fail $ "Identifier not in scope: " ++ v
      Just name -> return name

-- | Private newtype to key the TH state.
newtype IJState = IJState { methodCount :: Integer }

initialIJState :: IJState
initialIJState = IJState 0

getIJState :: Q IJState
getIJState = TH.getQ >>= \case
    Nothing -> do
      TH.putQ initialIJState
      return initialIJState
    Just st -> return st

setIJState :: IJState -> Q ()
setIJState = TH.putQ

-- | Declares /import/ statements to be included in the java compilation unit.
-- e.g.
--
-- > imports "java.util.*"
--
imports :: String -> Q [TH.Dec]
imports imp = do
    tJI <- [t| JavaImport |]
    lineNumber <- fromIntegral . fst . TH.loc_start <$> TH.location
    expJI <- TH.lift (JavaImport imp lineNumber)
    TH.addTopDecls
      -- {-# ANN module (JavaImport imp :: JavaImport) #-}
      [ TH.PragmaD $ TH.AnnP TH.ModuleAnnotation (TH.SigE expJI tJI) ]
    return []

-- | Yields the next method index. A different index is produced per call.
nextMethodIdx :: Q Integer
nextMethodIdx = do
    ij <- getIJState
    setIJState $ ij { methodCount = methodCount ij + 1 }
    return $ methodCount ij

-- | Idempotent action that loads all wrappers in every module of the current
-- program into the JVM. You shouldn't need to call this yourself.
loadJavaWrappers :: IO ()
loadJavaWrappers = doit `seq` return ()
  where
    {-# NOINLINE doit #-}
    doit = unsafePerformIO $ push $ do
      loader :: J ('Class "java.lang.ClassLoader") <- do
        thr <- callStatic "java.lang.Thread" "currentThread" []
        call (thr :: J ('Class "java.lang.Thread")) "getContextClassLoader" []
      forEachDotClass $ \name bc -> do
        _ <- defineClass (referenceTypeName (SClass name)) loader bc
        return ()
      pop

mangle :: TH.Module -> String
mangle (TH.Module (TH.PkgName pkgname) (TH.ModName mname)) =
    mangleClassName pkgname mname

blockOrExpQQ :: String -> Q TH.Exp
blockOrExpQQ txt@(words -> toks) -- ignore whitespace
  | ["{"] `isPrefixOf` toks
  , ["}"] `isSuffixOf` toks = blockQQ txt
  | otherwise = expQQ txt

expQQ :: String -> Q TH.Exp
expQQ input = blockQQ $ "{ return " ++ input ++ "; }"

blockQQ :: String -> Q TH.Exp
blockQQ input = case Java.parser Java.block input of
    Left err -> fail $ show err
    Right block -> do
      idx <- nextMethodIdx
      let mname = "inline__method_" ++ show idx
          vnames = antis block
      thnames <- mapM getValueName vnames

      -- Return a call to the static method we just generated.
      let args = [ [| coerce $(TH.varE name) |] | name <- thnames ]
      thismod <- TH.thisModule
      lineNumber <- fromIntegral . fst . TH.loc_start <$> TH.location
      [| loadJavaWrappers >>
         qqMarker
             (Proxy :: Proxy $(TH.litT $ TH.strTyLit input))
             (Proxy :: Proxy $(TH.litT $ TH.strTyLit mname))
             (Proxy :: Proxy $(TH.litT $ TH.strTyLit $ intercalate "," vnames))
             (Proxy :: Proxy $(TH.litT $ TH.numTyLit $ lineNumber))
             $(return $ foldr (\a b -> TH.TupE [TH.VarE a, b]) (TH.TupE []) thnames)
             Proxy
             (callStatic
             (fromString $(TH.stringE ("io.tweag.inlinejava." ++ mangle thismod)))
             (fromString $(TH.stringE mname))
             $(TH.listE args)) |]
