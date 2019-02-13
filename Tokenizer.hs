{-# LANGUAGE FlexibleContexts #-}

module Tokenizer where

import Text.Parsec
import Data.Either


data Token = Ident String
           | KeyWord String
           | Number Int  -- Change Number type if you work with something other than Int
           deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize input = fromRight [] $ parse (many getToken) "" input 

getToken :: Stream s m Char => ParsecT s u m Token
getToken = let gaps = many $ char ' ' in 
    try (gaps *> parseKeyWord)


parseKeyWord :: Stream s m Char => ParsecT s u m Token
parseKeyWord = KeyWord <$> choice (try . string <$> keyWords)
    where
        keyWords = ["False", "await", "else", "import", "pass",
                "None", "break", "except", "in", "raise",
                "True", "class", "finally", "is", "return",
                "and", "continue",  "for",       "lambda",    "try",
                "as",         "def",        "from",       "nonlocal",   "while",
                "assert",    "del",       "global",    "not", "with",
                "async",      "elif",       "if",         "or",         "yield"]


                
