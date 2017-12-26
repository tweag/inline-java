-- | This plugin generates Java bytecode from modules using the java
-- QuasiQuoter and inserts it in a global bytecode table from where the
-- it is loaded at runtime.
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Java.Inline.Plugin (plugin) where

import Control.Monad.Writer hiding ((<>))
import Convert (thRdrNameGuesses)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.Char (chr, ord)
import Data.Data (Data)
import Data.List (intersperse, isSuffixOf)
import Data.Maybe (mapMaybe)
import Data.Monoid (Endo(..))
import Data.IORef (readIORef)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import ErrUtils (ghcExit)
import FamInstEnv (normaliseType)
import Foreign.JNI.Types (JType(..))
import GhcPlugins
import IfaceEnv (lookupOrigNameCache)
import qualified Language.Haskell.TH as TH
import Language.Java.Inline.Magic
#if MIN_VERSION_ghc(8, 2, 1)
import NameCache (nsNames)
#endif
import TyCoRep
import TysWiredIn (nilDataConName, consDataConName)
import System.Directory (listDirectory)
import System.FilePath ((</>), (<.>))
import System.IO (withFile, IOMode(WriteMode), hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)

-- The 'java' quasiquoter produces annotations of type 'JavaImport', and it also
-- inserts calls in the code to the function 'qqMarker'.
--
-- 'qqMarker' carries many bits of information that are useful in generating the
-- QQ code.
--
-- This plugin first makes a pass to collect the 'qqMarker' calls in the module
-- (collectQQMarkers).
-- Then it generates the java stubs from the information in the found
-- occurrences (buildJava).
-- Finally, it calls the java compiler to produce the bytecode and
-- arranges it to be inserted it in the bytecode table in constructor functions
-- (cConstructors).

plugin :: Plugin
plugin = defaultPlugin
    { installCoreToDos = install
    }
  where
    install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
    install args todo = do
#if !MIN_VERSION_ghc(8,2,1)
      reinitializeGlobals
#endif
      return (CoreDoPluginPass "inline-java" (qqPass args) : todo)

    qqPass :: [CommandLineOption] -> ModGuts -> CoreM ModGuts
    qqPass args guts = do
      findTHName 'qqMarker >>= \case
        -- If qqMarker cannot be found we assume the module does not use
        -- inline-java.
        Nothing -> return guts
        Just qqMarkerName -> do
          (binds, qqOccs) <- collectQQMarkers qqMarkerName (mg_binds guts)
          let jimports = getModuleAnnotations guts :: [JavaImport]
          dcs <- buildJava guts qqOccs jimports
                   >>= maybeDumpJava args
                   >>= buildBytecode
          return guts
            { mg_binds = binds
            , mg_foreign = appendStubC (mg_foreign guts) $
                             text bctable_header
                           $$ dotClasses dcs
                           $$ cConstructors
            }

    -- The contents of bctable.h
    --
    -- #include "bctable.h" wouldn't work when ghc is used from the
    -- command line without saying -package inline-java.
    bctable_header :: String
    bctable_header = unlines
      [ "#include <stdlib.h>"
      , ""
      , "/** List of JVM class bytecode definitions. The content corresponds to"
      , " * that of a .class file. */"
      , "struct inline_java_dot_class {"
      , "  char *name;"
      , "  size_t bytecode_sz;"
      , "  unsigned char *bytecode;"
      , "};"
      , ""
      , ""
      , "/** A set of .class files contents. */"
      , "struct inline_java_pack {"
      , "  struct inline_java_pack *next;"
      , "  struct inline_java_dot_class *classes;"
      , "  size_t size;"
      , "};"
      , ""
      , "/** Smart constructor for class file packs. */"
      , "struct inline_java_pack *inline_java_new_pack("
      , "  struct inline_java_pack *next,"
      , "  struct inline_java_dot_class classes[],"
      , "  size_t size);"
      , ""
      , "/** Global list of class files."
      , " *"
      , " * All modules insert the bytecode of the classes they need when they"
      , " * are loaded."
      , " *"
      , " * inline-java reads this table to load the classes in loadJavaWrappers."
      , " */"
      , "extern struct inline_java_pack *inline_java_bctable;"
      ]

    -- Dumps the java code to stderr or a file, depending on the set flags.
    maybeDumpJava :: [CommandLineOption] -> Builder -> CoreM Builder
    maybeDumpJava args b
      | elem "dump-java" args = do
        dflags <- getDynFlags
        if gopt Opt_DumpToFile dflags then do
          thisModule <- getModule
          let fname = moduleNameString (moduleName thisModule) ++ ".dump-java"
              path = maybe fname (</> fname) (dumpDir dflags)
          liftIO $ withFile path WriteMode $ \h -> Builder.hPutBuilder h b
        else liftIO $ do
          hPutStrLn stderr "=== inline-java (dump-java) BEGIN ==="
          Builder.hPutBuilder stderr b
          hPutStrLn stderr "=== inline-java (dump-java) END ==="
        return b
    maybeDumpJava _ b = return b

-- | Produces a Java compilation unit from the quasiquotation occurrences and
-- the java imports.
--
-- The compilation unit looks like:
--
-- > package io.tweag.inlinejava;
-- > import java.util.*; // .hs:25
-- >
-- > public final class Inline__<pkgname>_<modname> {
-- > public static java.lang.Object method_0()
-- > { return  1 + 1 ; } // .hs:31
-- > public static java.lang.Object inline__method_1()
-- > { // .hs:34
-- >              int x = 1; // .hs:34
-- >              int y = 2; // .hs:34
-- >              return x + y; // .hs:34
-- >            } // .hs:34
-- > public static java.lang.Object inline__method_2(final int $x)
-- > { return  $x + 1 ; } // .hs:42
-- > public static java.lang.Object inline__method_3()
-- > { return  new Object() {} ; } // .hs:46
-- > }
--
-- Where @inline_method_i@ is the method corresponding to the @ith@
-- quasiquotation.
buildJava :: ModGuts -> [QQOcc] -> [JavaImport] -> CoreM Builder
buildJava guts qqOccs jimports = do
    let importsJava = mconcat
          [ mconcat [ "import ", Builder.stringUtf8 jimp
                    , "; // .hs:", Builder.integerDec n
                    , "\n"
                    ]
          | JavaImport jimp n <- jimports
          ]
    p_fam_env <- getPackageFamInstEnv
    let fam_envs = (p_fam_env, mg_fam_inst_env guts)
    methods <- forM qqOccs $ \QQOcc {..} -> do
      let (_, normty) = normaliseType fam_envs Nominal (expandTypeSynonyms qqOccResTy)
      jTypeNames <- findJTypeNames
      resty <- case toJavaType jTypeNames normty of
        Just resty -> return resty
        Nothing -> failWith $ hsep
          [ parens (text "line" <+> integer qqOccLineNumber) <> ":"
          , text "The result type of the quasiquotation"
          , quotes (ppr qqOccResTy)
          , text "is not sufficiently instantiated to infer a java type."
          ]
      let argnames = BS.split (fromIntegral $ fromEnum ',') qqOccAntiQs
      argtys <- zipWithM (getArg jTypeNames qqOccLineNumber)
                         argnames qqOccArgTys
      return $ mconcat
        [ "public static "
        , Builder.byteString resty
        , " "
        , Builder.byteString qqOccMName
        , "("
        , mconcat $ intersperse "," argtys
        , ") throws Throwable // .hs:"
        , Builder.integerDec qqOccLineNumber
        , "\n"
        , adjustInput qqOccLineNumber qqOccInput
        ]
    thisModule <- getModule
    let className = mangle thisModule
    return $ mconcat
      [ "package io.tweag.inlinejava;\n"
      , importsJava
      , "\n"
      , "public final class "
      , Builder.stringUtf8 className
      , " {\n"
      , mconcat methods
      , "}\n"
      ]
  where
    getArg :: JTypeNames -> Integer -> BS.ByteString -> Type -> CoreM Builder
    getArg jTypeNames _ name
           (expandTypeSynonyms -> toJavaType jTypeNames -> Just jtype) =
      return $ mconcat
        ["final ", Builder.byteString jtype, " $", Builder.byteString name]
    getArg _ line name t = failWith $ hsep
        [ parens (text "line" <+> integer line) <> ":"
        , quotes (ftext (mkFastStringByteString name) <+> "::" <+> ppr t)
        , text "is not sufficiently instantiated to infer a java type."
        ]

    adjustInput :: Integer -> BS.ByteString -> Builder
    adjustInput lineNumber bs =
      let txt = Text.strip $ Text.decodeUtf8 bs
          block = if Text.isPrefixOf "{" txt && Text.isSuffixOf "}" txt
            then Text.lines txt
            else Text.lines $ Text.concat ["{ return ", txt, "}" ]

       in Builder.byteString $ Text.encodeUtf8 $ Text.unlines
            [ Text.concat [ln, " // .hs:", Text.pack (show lineNumber)]
            | ln <- block
            ]

-- | Produces a class name from a Module.
mangle :: Module -> String
mangle m = mangleClassName (unitIdString (moduleUnitId m))
                           (moduleNameString (moduleName m))

-- Call the java compiler and feeds it the given Java code in Builder form.
buildBytecode :: Builder -> CoreM [DotClass]
buildBytecode unit = do
    m <- getModule
    liftIO $ withSystemTempDirectory "inlinejava" $ \dir -> do
      let src = dir </> mangle m <.> "java"
      withFile src WriteMode $ \h -> Builder.hPutBuilder h unit
      callProcess "javac" [src]
      -- A single compilation unit can produce multiple class files.
      classFiles <- filter (".class" `isSuffixOf`) <$> listDirectory dir
      forM classFiles $ \classFile -> do
        bcode <- BS.readFile (dir </> classFile)
        -- Strip the .class suffix.
        let klass = "io.tweag.inlinejava." ++ takeWhile (/= '.') classFile
        return $ DotClass klass bcode

-- | The names of 'JType' data constructors
data JTypeNames = JTypeNames
    { nameClass :: Maybe Name
    , nameIface :: Maybe Name
    , nameArray :: Maybe Name
    , nameGeneric :: Maybe Name
    , namePrim :: Maybe Name
    , nameVoid :: Maybe Name
    }

-- | Produces the names of the data constructors of the 'JType'
-- if they are used in the current module.
findJTypeNames :: CoreM JTypeNames
findJTypeNames = do
    nameClass <- findTHName 'Class
    nameIface <- findTHName 'Iface
    nameArray <- findTHName 'Array
    nameGeneric <- findTHName 'Generic
    namePrim <- findTHName 'Prim
    nameVoid <- findTHName 'Void
    return $ JTypeNames {..}

-- | Produces a java type from a Core 'Type' if the type is sufficiently
-- instantiated and it is of kind 'JType'.
toJavaType :: JTypeNames -> Type -> Maybe BS.ByteString
toJavaType JTypeNames {..} t0 = BS.concat <$> go t0
  where
    go :: Type -> Maybe [BS.ByteString]
    go (TyConApp c [LitTy (StrTyLit fs)])
      | Just n <- nameClass, tyConName c == n =
        Just [substDollar $ fastStringToByteString fs]
      | Just n <- nameIface, tyConName c == n =
        Just [substDollar $ fastStringToByteString fs]
    go (TyConApp c [t])
      | Just n <- nameArray, tyConName c == n =
        (++ ["[]"]) <$> go t
    go (TyConApp c [t, ts])
      | Just n <- nameGeneric, tyConName c == n = do
        bs <- go t
        args_ts <- listGo ts
        Just $ bs ++ "<" : concat (intersperse [","] args_ts) ++ [">"]
    go (TyConApp c [])
      | Just n <- nameVoid, tyConName c == n =
        Just ["void"]
    go (TyConApp c [LitTy (StrTyLit fs)])
      | Just n <- namePrim, tyConName c == n =
        Just [fastStringToByteString fs]
    go _ = Nothing

    listGo :: Type -> Maybe [[BS.ByteString]]
    listGo (TyConApp c [_]) | nilDataConName == tyConName c = Just []
    listGo (TyConApp c [_, tx, txs]) | consDataConName == tyConName c =
      (:) <$> go tx <*> listGo txs
    listGo _ = Nothing

    -- Substitutes '$' with '.' in java names.
    substDollar :: BS.ByteString -> BS.ByteString
    substDollar xs
      | fromIntegral (ord '$') `BS.elem` xs =
        let subst (chr . fromIntegral -> '$') = fromIntegral (ord '.')
            subst x = x
         in BS.map subst xs
      | otherwise = xs

-- | An occurrence of 'qqMarker'
data QQOcc = QQOcc
    { -- | The type of the result
      qqOccResTy :: Type
      -- | The type of the arguments
    , qqOccArgTys :: [Type]
      -- | The input of the quasiquoter
    , qqOccInput :: BS.ByteString
      -- | The name of the method to generate
    , qqOccMName :: BS.ByteString
      -- | The antiquotations of the quasiquoter
    , qqOccAntiQs :: BS.ByteString
      -- | The line number where the quasiquotation appears
    , qqOccLineNumber :: Integer
    }

-- | A monad for collecting qqMarker occurrences.
type QQJavaM a = WriterT (Endo [QQOcc]) CoreM a

-- Collects the occurrences of qqMarkers.
--
-- The program is expected to have 'qqMarker' occurrences inserted
-- by the java quasiquoter.
--
-- > module A where
-- > import Language.Java.Inline
-- >
-- > f = ...
-- >     (qqMarker ... (callStatic "Inline__<pkg>_<mod>" "inline__method_i" []))
-- >     ...
-- >
-- > g = ...
-- > ...
--
-- 'collectQQMarkers' yields one 'QQOcc' value for every occurrences of
-- 'qqMarker', and the program resulting from removing the markers.
--
-- > module A where
-- > import Language.Java.Inline
-- >
-- > f = ...
-- >     (callStatic "Inline__<pkg>_<mod>" "inline__method_i" [])
-- >     ...
-- >
-- > g = ...
-- > ...
--
collectQQMarkers
  :: Name -> CoreProgram -> CoreM (CoreProgram, [QQOcc])
collectQQMarkers qqMarkerName p0 = do
    (p1, e) <- runWriterT (mapM bindMarkers p0)
    return (p1, appEndo e [])
  where
    bindMarkers :: CoreBind -> QQJavaM CoreBind
    bindMarkers (NonRec b e) = NonRec b <$> expMarkers e
    bindMarkers (Rec bs) = Rec <$> mapM (\(b, e) -> (,) b <$> expMarkers e) bs

    expMarkers :: CoreExpr -> QQJavaM CoreExpr
    expMarkers (App (App (App (App (App (App (App (App (App (App (App (App (App
                 (App (App (App (App (App (App (App (Var fid) _)
                 (Type (parseArgTys -> Just tyargs)))
                 (Type tyres))
                 (Type (LitTy (StrTyLit fs_input))))
                 (Type (LitTy (StrTyLit fs_mname))))
                 (Type (LitTy (StrTyLit fs_antiqs))))
                 (Type (LitTy (NumTyLit lineNumber))))
                 _) _) _) _) _) _) _) _) _) _) _) _)
                 e
               )
        | qqMarkerName == idName fid = do
      tell $ Endo $ (:) $ QQOcc
        { qqOccResTy = tyres
        , qqOccArgTys = tyargs
        , qqOccInput = fastStringToByteString fs_input
        , qqOccMName = fastStringToByteString fs_mname
        , qqOccAntiQs = fastStringToByteString fs_antiqs
        , qqOccLineNumber = lineNumber
        }
      return e
    expMarkers (Var fid) | qqMarkerName == idName fid = lift $ failWith $
      text "inline-java Plugin: found invalid qqMarker."
    expMarkers (App e a) = App <$> expMarkers e <*> expMarkers a
    expMarkers (Lam b e) = Lam b <$> expMarkers e
    expMarkers (Let bnd e) = Let <$> bindMarkers bnd <*> expMarkers e
    expMarkers (Case e0 b t alts) = do
      e0' <- expMarkers e0
      let expAlt (a, bs, e) = (,,) a bs <$> expMarkers e
      alts' <- mapM expAlt alts
      return (Case e0' b t alts')
    expMarkers (Cast e c) = flip Cast c <$> expMarkers e
    expMarkers (Tick t e) = Tick t <$> expMarkers e
    expMarkers e@(Coercion {}) = return e
    expMarkers e@(Lit {}) = return e
    expMarkers e@(Var {}) = return e
    expMarkers e@(Type {}) = return e

    -- @parseArgTys (t_1, (t_2, ... (t_n, ()) ... )) = Just [t_1, t_2, ... t_n]@
    --
    -- Yields @Nothing@ when the input is not of the expected form.
    parseArgTys :: Type -> Maybe [Type]
    parseArgTys (TyConApp c [_, _, ty, tys])
      | c == promotedTupleDataCon Boxed 2 =
        (ty:) <$> parseArgTys tys
    parseArgTys (TyConApp c [])
      | c == unitTyCon =
        Just []
    parseArgTys _ = Nothing

-- | Produces static structures which contain the class names and
-- the bytecodes. For instance:
--
-- > static unsigned char bc0[] = {202, 254, ... }
-- >
-- > static unsigned char bc1[] = {202, 254, ... }
-- >
-- > static struct inline_java_dot_class dcs[] =
-- >   { { "io.tweag.inlinejava.Inline__main_Language_Java_InlineSpec"
-- >     , 2941 // length of bc0
-- >     , bc0
-- >     }
-- >  , { "io.tweag.inlinejava.Inline__main_Language_Java_InlineSpec$1Foo"
-- >    , 579 // length of bc1
-- >    , bc1
-- >    }
-- > };
-- >
dotClasses :: [DotClass] -> SDoc
dotClasses dcs = vcat $
      text "static int dc_count =" <+> int (length dcs) <> semi
    : [ vcat
        [ text "static unsigned char bc" <> int i <> text "[] ="
        , braces (pprWithCommas (text . show) (BS.unpack bc)) <> semi
        ]
      | (i, DotClass _ bc) <- zip [0..] dcs
      ]
      ++
      [ text "static struct inline_java_dot_class dcs[] ="
      , braces
          (pprWithCommas
            (\(i, DotClass name bc) ->
               braces $ pprWithCommas id
                 [ text (show name), int (BS.length bc), text "bc" <> int i])
            (zip [0..] dcs)
          ) <> semi
      ]

-- | Produces the constructor function which inserts the static structures
-- generated by 'dotClasses' into the bytecode table.
cConstructors :: SDoc
cConstructors = vcat
    [ text "static void hs_inline_java_init(void) __attribute__((constructor));"
    , text "static void hs_inline_java_init(void)"
    , text "{ inline_java_bctable = inline_java_new_pack(inline_java_bctable, dcs, dc_count); }"
    ]

--------------------------------------------
-- Candidates for addition to GhcPlugins
--------------------------------------------

-- | Produces a name in GHC Core from a Template Haskell name.
--
-- Yields Nothing if the name can't be found, which may happen if the
-- module defining the named thing hasn't been loaded.
findTHName :: TH.Name -> CoreM (Maybe Name)
findTHName th_name =
    case thRdrNameGuesses th_name of
      Orig m occ : _ -> do
        hsc_env <- getHscEnv
        nc <- liftIO $ readIORef (hsc_NC hsc_env)
        return $ lookupOrigNameCache (nsNames nc) m occ
      _ -> return Nothing

-- | Yields module annotations with values of the given type.
getModuleAnnotations :: Data a => ModGuts -> [a]
getModuleAnnotations guts =
    mapMaybe (fromSerialized deserializeWithData)
      [ v | Annotation (ModuleTarget _) v <- mg_anns guts ]

-- | Prints the given error message and terminates ghc.
failWith :: SDoc -> CoreM a
failWith m = do
    errorMsg m
    dflags <- getDynFlags
    liftIO $ ghcExit dflags 1
    return (error "ghcExit returned!?") -- unreachable
