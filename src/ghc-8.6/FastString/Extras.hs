module FastString.Extras(bytesFS) where

import Data.ByteString
import qualified FastString as FastString

bytesFS :: FastString.FastString -> ByteString
bytesFS = FastString.fastStringToByteString
