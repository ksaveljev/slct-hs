{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (foldl')
import Data.Word (Word32, Word64)
import System.IO (Handle, hIsEOF, stdin)

import SLCT.Constants
import qualified SLCT.ByteStringHash as BSHash

-- TODO: consider a hash with better distribution
hash :: ByteString -> Word64
hash = BSHash.shiftAddXor

splitWords :: ByteString -> [ByteString]
splitWords = C.split ' '

populate :: Map Word64 Word32 -> Handle -> IO (Map Word64 Word32)
populate hashes h = do
    eof <- hIsEOF h
    if eof then return hashes
           else do
             l <- BS.hGetLine h
             let ws = splitWords l
             let hashes' = foldl' (\m w -> Map.insertWith (+) (hash w) 1 m) hashes ws
             populate hashes' h

main :: IO()
main = do
    m <- populate Map.empty stdin
    putStr $ show $ Map.size m
