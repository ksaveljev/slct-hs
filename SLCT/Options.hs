module SLCT.Options where

data Options = Options { byteOffset :: Int
                       , clusterTableSize :: Int
                       , delimiterRegex :: Maybe String
                       , filterRegex :: Maybe String
                       , sliceSize :: Int
                       , initialSeed :: Int
                       , join :: Bool
                       , outliersFile :: Maybe String
                       , refine :: Bool
                       , support :: Double
                       , template :: Maybe [String]
                       , vectorSize :: Int
                       , wordTableSize :: Int
                       , clusterVectorSize :: Int
                       }

options :: Options
options = Options { byteOffset = 0
                  , clusterTableSize = 0
                  , delimiterRegex = Nothing
                  , filterRegex = Nothing
                  , sliceSize = 0
                  , initialSeed = 1
                  , join = False
                  , outliersFile = Nothing
                  , refine = False
                  , support = 0
                  , template = Nothing
                  , vectorSize = 0
                  , wordTableSize = 100000
                  , clusterVectorSize = 0
                  }
