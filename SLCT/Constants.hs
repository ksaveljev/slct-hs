module SLCT.Constants
  ( constants
  , maxLineLength
  , maxWordLength
  , maxWordsPerLine
  , maxHashKeyLength
  , clusterSeparator
  , maxLogMessageLength
  , backReferenceChar
  , maxParenthesesInRegex
  ) where

data Constants = Constants { _maxLineLength :: Int         -- maximum length of a line
                           , _maxWordLength :: Int         -- maximum length of a word, must be at least maxLineLength+4 
                           , _maxWordsPerLine :: Int       -- maximum number of words in one line (must not exceed 2^16-1)
                           , _maxHashKeyLength :: Int      -- maximum hash key length in cluster hash table
                           , _clusterSeparator :: Char     -- separator character used for building hash keys of the cluster hash table
                           , _maxLogMessageLength :: Int   -- maximum log message length
                           , _backReferenceChar :: Char    -- character that starts backreference variables
                           , _maxParenthesesInRegex :: Int -- maximum number of () expression in regex
                           }

-- we do not want our constants to be editable outside of this module

maxLineLength :: Constants -> Int
maxLineLength = _maxLineLength

maxWordLength :: Constants -> Int
maxWordLength = _maxWordLength

maxWordsPerLine :: Constants -> Int
maxWordsPerLine = _maxWordsPerLine

maxHashKeyLength :: Constants -> Int
maxHashKeyLength = _maxHashKeyLength

clusterSeparator :: Constants -> Char
clusterSeparator = _clusterSeparator

maxLogMessageLength :: Constants -> Int
maxLogMessageLength = _maxLogMessageLength

backReferenceChar :: Constants -> Char
backReferenceChar = _backReferenceChar

maxParenthesesInRegex :: Constants -> Int
maxParenthesesInRegex = _maxParenthesesInRegex

constants :: Constants
constants = Constants { _maxLineLength = 10240
                      , _maxWordLength = 10248
                      , _maxWordsPerLine = 512
                      , _maxHashKeyLength = 20480
                      , _clusterSeparator = '\n'
                      , _maxLogMessageLength = 256
                      , _backReferenceChar = '$'
                      , _maxParenthesesInRegex = 100
                      }
