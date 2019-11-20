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

module Language.Java.Inline.Internal
  ( javaWithConfig
  , QQConfig(..)
  , imports
  , loadJavaWrappers
  ) where

import Control.Monad (when)
import Data.Data
import Data.List (isPrefixOf, intercalate, isSuffixOf, nub)
import Data.String (fromString)
import Foreign.JNI (defineClass)
import Language.Java
import Language.Java.Internal (maxVariadicArgs)
import Language.Java.Inline.Internal.Magic as Magic
import qualified Language.Java.Lexer as Java
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
-- Then, we inject a call to 'qqMarker' which carries the needed types to the
-- plugin phase.
--
-- The plugin phase is implemented in Language.Java.Inline.Plugin. In this phase
-- we make a pass over the module Core to find all the occurrences of
-- 'qqMarker'. By this point the types of all the variables in the local scope
-- that was captured are fully determined. So we can analyze these types to
-- determine what the signature of the wrapper should be, in order to declare
-- it.
--
-- The last step is to ask the Java toolchain to produce .class bytecode from
-- our declarations. We embed this bytecode in the binary, adding a reference to
-- it in a global bytecode table. That way at runtime we can enumerate
-- the bytecode blobs, and load them into the JVM one by one.

javaWithConfig :: QQConfig -> QuasiQuoter
javaWithConfig config = QuasiQuoter
    { quoteExp = \txt -> blockOrExpQQ config txt
    , quotePat  = error "Language.Java.Inline: quotePat"
    , quoteType = error "Language.Java.Inline: quoteType"
    , quoteDec  = error "Language.Java.Inline: quoteDec"
    }

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
    tJI <- [t| Magic.JavaImport |]
    lineNumber <- fromIntegral . fst . TH.loc_start <$> TH.location
    expJI <- TH.lift (Magic.JavaImport imp lineNumber)
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
        thr <- callStatic "java.lang.Thread" "currentThread"
        call (thr :: J ('Class "java.lang.Thread")) "getContextClassLoader"
      Magic.forEachDotClass $ \Magic.DotClass{..} -> do
        _ <- defineClass (referenceTypeName (SClass className)) loader classBytecode
        return ()
      pop

mangle :: TH.Module -> String
mangle (TH.Module (TH.PkgName pkgname) (TH.ModName mname)) =
    Magic.mangleClassName pkgname mname

-- | Customizes how quasiquotations are desugared.
data QQConfig = QQConfig
  { -- | This is the name of the function to use to indicate to the
    -- plugin the presence of a java quasiquotation.
    qqMarker :: TH.Name
    -- | This is the name of the function to use to invoke the Java stub.
  , qqCallStatic :: TH.Name
    -- | This is the name of the function to use for coercing the values of
    -- antiquotations to `JValues`.
  , qqCoerce :: TH.Name
    -- | This is given as argument the invocation of the Java stub, and
    -- is expected to prepend it with code that ensures that the stub is
    -- previously loaded in the JVM.
  , qqWrapMarker :: TH.ExpQ -> TH.ExpQ
  }

blockOrExpQQ :: QQConfig -> String -> Q TH.Exp
blockOrExpQQ config txt@(words -> toks) -- ignore whitespace
  | ["{"] `isPrefixOf` toks
  , ["}"] `isSuffixOf` toks = blockQQ config txt
  | otherwise = expQQ config txt

expQQ :: QQConfig ->String -> Q TH.Exp
expQQ config input = blockQQ config $ "{ return " ++ input ++ "; }"

blockQQ :: QQConfig -> String -> Q TH.Exp
blockQQ config input = do
      idx <- nextMethodIdx
      when (idx == 0) $
        TH.addCorePlugin "Language.Java.Inline.Plugin"
      let mname = "inline__method_" ++ show idx
          vnames = nub
            [ n | Java.L _ (Java.IdentTok ('$' : n)) <- Java.lexer input ]
          thnames = map TH.mkName vnames
          thnames' = map TH.mkName (map ('_':) vnames)
      -- Keep consistent with number of instances generated Language.Java.Internal.
      when (length vnames > maxVariadicArgs) $
        TH.reportError "Blocks with more than 32 antiquotation variables not supported."

      -- Return a call to the static method we just generated.
      thismod <- TH.thisModule
      lineNumber <- fromIntegral . fst . TH.loc_start <$> TH.location
      qqWrapMarker config
        [| $(TH.varE (qqMarker config))
             (Proxy :: Proxy $(TH.litT $ TH.strTyLit input))
             (Proxy :: Proxy $(TH.litT $ TH.strTyLit mname))
             (Proxy :: Proxy $(TH.litT $ TH.strTyLit $ intercalate "," vnames))
             (Proxy :: Proxy $(TH.litT $ TH.numTyLit $ lineNumber))
             $(return $ foldr (\a b -> TH.TupE [TH.VarE a, b]) (TH.TupE []) thnames)
             Proxy
             (\ $(return $ foldr (\a b -> TH.TupP [TH.VarP a, b]) (TH.TupP []) thnames') ->
                $(TH.appsE
                    ([ TH.varE (qqCallStatic config)
                     , [| fromString $(TH.stringE ("io.tweag.inlinejava." ++ mangle thismod)) |]
                     , [| fromString $(TH.stringE mname) |]
                     ] ++ map TH.varE thnames')
                 )
             )
             |]
