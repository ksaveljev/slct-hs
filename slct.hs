{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.List (foldl')
import Data.Word (Word32)
import System.IO (Handle, hIsEOF, stdin)
import Options.Applicative

import SLCT.Options
import qualified SLCT.ByteStringHash as BSHash

type Hash = Int
type HashCount = IntMap Word32

hash :: ByteString -> Hash
{-# INLINE hash #-}
hash = fromIntegral . BSHash.fnv1a64

splitWords :: ByteString -> [ByteString]
{-# INLINE splitWords #-}
splitWords = C.split ' '

populate :: HashCount -> Handle -> IO HashCount
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
    print =<< execParser (optionsParser `withInfo` "SLCT-hs version 0.1.0.0, Copyright AUTHORS")
    m <- populate Map.empty stdin
    putStr $ show $ Map.size m
    where
      withInfo opts desc = info (helper <*> opts) $ progDesc desc
