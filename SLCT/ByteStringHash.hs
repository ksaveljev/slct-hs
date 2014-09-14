module SLCT.ByteStringHash (
  fnv1a64,
  shiftAddXor,
) where

import Data.Bits (unsafeShiftL, unsafeShiftR, xor)
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
{-# INLINE shiftAddXor #-}
shiftAddXor = BS.foldl' hash 0
    where
      hash :: Word64 -> Word8 -> Word64
      hash h ch = h `xor` (unsafeShiftL h 5 + unsafeShiftR h 2 + fromIntegral ch)

-- FNV-1a hashing function
-- FNV Prime for 64 bits: 2^40 + 2^8 + 0xb3
fnvPrime64 :: Word64
fnvPrime64 = 1099511628211

fnv1OffsetBasis64 :: Word64
fnv1OffsetBasis64 = 14695981039346656037

fnv1a64 :: ByteString -> Word64
{-# INLINE fnv1a64 #-}
fnv1a64 = BS.foldl' hash fnv1OffsetBasis64
    where
      hash :: Word64 -> Word8 -> Word64
      hash h ch = (h `xor` fromIntegral ch) * fnvPrime64
