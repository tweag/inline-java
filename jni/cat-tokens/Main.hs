-- | Takes three file paths, and copies the file at the second location to the
-- third location, replacing occurrences of #### with the empty string.
--
-- The first location is used to prepend the output with a line pragma.
--
-- > {-# LINE 1 "firstLocation" #-}
--
--
-- This is a script to concatenate tokens after CPP preprocessing.
--
-- On OSX we used to rely on cpphs to concatenate tokens in
-- definitions like
--
-- > define GET_FIELD(name, hs_rettype, c_rettype) \
-- >   get/**/name/**/Field :: Coercible o (J a) => o -> JFieldID -> IO hs_rettype;
--
-- The C preprocessor in OSX would otherwise replace the
-- comments with whitespaces.
--
-- Using cpphs, however, required a couple of hacks to workaround
-- https://github.com/haskell/cabal/issues/4278
-- https://gitlab.haskell.org/ghc/ghc/-/issues/17185
--
-- And moreover, when using rules_haskell, ghc passes response files to cpphs,
-- which are unsupported.
-- https://github.com/tweag/rules_haskell/pull/836
--
-- Therefore, we currently resolve concatenation with a custom
-- preprocessor to avoid the pile of hacks. The concatenator operator is ####.
--
-- > define GET_FIELD(name, hs_rettype, c_rettype) \
-- >   get####name####Field :: Coercible o (J a) => o -> JFieldID -> IO hs_rettype;
--
module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import System.Environment
import System.IO

main :: IO ()
main = do
   [srcF, inF, outF] <- getArgs
   bs <- BS.readFile inF
   hOut <- openBinaryFile outF WriteMode
   hPutStrLn hOut $ "{-# LINE 1 \"" ++ srcF ++ "\" #-}";
   mapM_ (BS.hPut hOut) $ tokenise bs
   hClose hOut

tokenise :: BS.ByteString -> [BS.ByteString]
tokenise y = h : if BS.null t then [] else tokenise (BS.drop 4 t)
    where (h, t) = BS.breakSubstring (C8.pack "####") y
