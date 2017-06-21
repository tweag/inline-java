-- | Internal module defining some magic, kept separate from the rest, that
-- depends on compiler internals.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}

module Language.Java.Inline.Magic
  ( DotClass(className, classBytecode)
  , mkDotClass
  , isDotClassStaticKey
  ) where

import qualified Data.ByteString as BS
import Foreign.Storable (sizeOf)
import GHC.Exts (Int(..), Word(..), Word#)
import GHC.Prim (indexWordArray#, sizeofByteArray#, unpackClosure#)
import GHC.StaticPtr (StaticPtr, deRefStaticPtr)
import qualified Language.Haskell.TH.Syntax as TH
import Prelude hiding (words)

-- | A wrapper for class bytecode. This datatype includes an unpacked
-- fingerprint. This fingerprint is used by 'isDotClassStaticKey' to
-- discriminate between static pointers to 'DotClass' values vs static pointers
-- to other values just by inspecting whether the closure contains the magic
-- values. Use 'mkDotClass' smart constructor to build.
data DotClass = DotClass
  { className :: String
  , classBytecode :: BS.ByteString
  , classMagicHigh :: Word#
  , classMagicLow :: Word#
  }

mkDotClass
  :: String -- ^ Class name
  -> BS.ByteString -- ^ Class bytecode
  -> DotClass
mkDotClass className classBytecode =
    let ![W# classMagicHigh, W# classMagicLow] = magic
    in DotClass{..}

instance TH.Lift DotClass where
  lift DotClass{..} =
      [| mkDotClass
           $(TH.lift className)
           (BS.pack $(TH.lift (BS.unpack classBytecode)))
       |]

-- | Two random numbers assumed to be unique.
magic :: [Word]
-- Explicit 'fromIntegral' to avoid literal overflow warnings on 32-bits
-- architectures.
magic =
    [ fromIntegral (9312424290567649534 :: Integer)
    , fromIntegral (8204450874354285209 :: Integer)
    ]

-- | Test whether the static key identifies a static pointer to a value of type
-- 'DotClass'.
isDotClassStaticKey :: StaticPtr a -> Bool
isDotClassStaticKey sptr = words == magic
  where
    wordSize = sizeOf (undefined :: Word)
    words =
      let !x = deRefStaticPtr sptr in
      case unpackClosure# x of
        -- Inspect non-pointer members of the closure only. If magic isn't
        -- unpacked properly, then we'll get false negatives.
        (# _, _, nptrs #) ->
          [ W# (indexWordArray# nptrs i)
          | I# i <- [0 .. (I# (sizeofByteArray# nptrs) `div` wordSize) - 1]
          ]
