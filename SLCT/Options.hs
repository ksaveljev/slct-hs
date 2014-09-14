module SLCT.Options where

import Options.Applicative

data Options = Options { byteOffset :: Int
                       , clusterTableSize :: Int
                       , delimiterRegex :: Maybe String
                       , filterRegex :: Maybe String
                       , sliceSize :: Int
                       , initialSeed :: Int
                       , allowJoin :: Bool
                       , outliersFile :: Maybe String
                       , refine :: Bool
                       , support :: Double
                       , template :: Maybe [String]
                       , vectorSize :: Int
                       , wordTableSize :: Int
                       , clusterVectorSize :: Int
                       , inputFiles :: [String]
                       } deriving (Show)

optionsParser :: Parser Options
optionsParser = Options
              <$> option auto (short 'b' <> value 0 <> metavar "BYTEOFFSET")
              <*> option auto (short 'c' <> value 0 <> metavar "CLUSTERTABLESIZE")
              <*> strOptional (short 'd' <> metavar "DELIM")
              <*> strOptional (short 'f' <> metavar "FILTER")
              <*> option auto (short 'g' <> value 0 <> metavar "SLICESIZE")
              <*> option auto (short 'i' <> value 1 <> metavar "INITSEED")
              <*> switch (short 'j')
              <*> strOptional (short 'o' <> metavar "OUTLIERFILE")
              <*> switch (short 'r')
              <*> option auto (short 's' <> value 0 <> metavar "PCTSUPPORT")
              <*> strMultiple (short 't' <> metavar "TEMPLATE")
              <*> option auto (short 'v' <> value 0 <> metavar "VECTORSIZE")
              <*> option auto (short 'w' <> value 100000 <> metavar "WORDTABLESIZE")
              <*> option auto (short 'z' <> value 0 <> metavar "CLUSTERVECTORSIZE")
              <*> strArguments (metavar "INPUT")

-- We could use `optional` from Control.Applicative but this is just to
-- learn the way optional works
strOptional :: Mod OptionFields String -> Parser (Maybe String)
strOptional flags = Just <$> strOption flags <|> pure Nothing

strMultiple :: Mod OptionFields String -> Parser (Maybe [String])
strMultiple flags = Just <$> many (strOption flags) <|> pure Nothing

strArguments :: Mod ArgumentFields String -> Parser [String]
strArguments = many . strArgument
