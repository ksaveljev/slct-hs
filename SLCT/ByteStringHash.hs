module SLCT.ByteStringHash (
  shiftAddXor
) where

import Data.Bits (shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word64)

-- a simple shift-add-xor hash
--
-- original C code:
--   for (i = 0; string[i] != 0; ++i) {
--       h = h ^ ((h << 5) + (h >> 2) + string[i]);
--   }
shiftAddXor :: ByteString -> Word64
shiftAddXor = BS.foldl' hash 0
    where
      hash :: Word64 -> Word8 -> Word64
      hash h ch = h `xor` (shiftL h 5 + shiftR h 2 + fromIntegral ch) 
