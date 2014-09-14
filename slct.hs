{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (foldM, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl', intersperse)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import Options.Applicative
import System.IO (Handle, IOMode(ReadMode), SeekMode(AbsoluteSeek), hIsEOF, hSeek, openFile)

import SLCT.Options
import qualified SLCT.ByteStringHash as BSHash

type Hash = Int
type Count = Word32
type Hashes = IntMap Count
type Clusters = Map ByteString Count

hash :: ByteString -> Hash
{-# INLINE hash #-}
hash = fromIntegral . BSHash.fnv1a64

splitWords :: ByteString -> [ByteString]
{-# INLINE splitWords #-}
splitWords = C.split ' '

clusterify :: Hashes -> Count -> [ByteString] -> ByteString
clusterify freq wf ws = BS.concat $ intersperse " " $ map (\w -> if frequent w then w else "*") ws
    where
      frequent :: ByteString -> Bool
      frequent w = case IntMap.lookup (hash w) freq of Just i -> i >= wf
                                                       Nothing -> False

wordFrequency :: Hashes -> Handle -> IO Hashes
wordFrequency hashes h = do
    eof <- hIsEOF h
    if eof then return hashes
           else do
             l <- BS.hGetLine h
             let ws = splitWords l
             let hashes' = foldl' (\m w -> IntMap.insertWith (+) (hash w) 1 m) hashes ws
             wordFrequency hashes' h

populateClusters :: Hashes -> Count -> Clusters -> Handle -> IO Clusters
populateClusters freq wf clusters h = do
    eof <- hIsEOF h
    if eof then return clusters
           else do
             l <- BS.hGetLine h
             let ws = splitWords l
             let cluster = clusterify freq wf ws
             populateClusters freq wf (Map.insertWith (+) cluster 1 clusters) h

main :: IO()
main = do
    opts <- execParser (optionsParser `withInfo` "SLCT-hs version 0.1.0.0, Copyright AUTHORS")
    print opts

    when (null $ inputFiles opts) $ error "No input files specified"

    inputs <- mapM (`openFile` ReadMode) (inputFiles opts)
    frequentWords <- foldM wordFrequency IntMap.empty inputs

    -- return to beginning of input files for a second pass
    mapM_ (\h -> hSeek h AbsoluteSeek 0) inputs
    clusters <- foldM (populateClusters frequentWords (fromIntegral $ minWordFreq opts)) Map.empty inputs

    -- print stats
    putStrLn $ "total distinct words: " ++ show (IntMap.size frequentWords)
    putStrLn $ "clusters: " ++ show (Map.size clusters)

    -- print all clusters with at least "--cf N" matches
    let filtered = Map.filter (>= (fromIntegral $ minClusterFreq opts)) clusters
    mapM_ (\(k,v) -> putStrLn (show v ++ "\t" ++ show k)) (Map.assocs filtered)
    where
      withInfo opts desc = info (helper <*> opts) $ progDesc desc
