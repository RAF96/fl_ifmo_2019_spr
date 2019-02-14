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
getToken =  
    try (spaces *> parseKeyWord) <|>
    try (spaces *> parseInteger) <|>
    try (spaces *> parseIdent) 


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

bar :: String -> Int
bar input = fst $ head $ (reads input)



parseInteger :: Stream s m Char => ParsecT s u m Token
parseInteger = Number <$> (bar <$> (many1 digit))


parseIdent :: Stream s m Char => ParsecT s u m Token
parseIdent = Ident <$> do
    res1 <- (string "_" <|> ((\x -> [x]) <$> letter))
    res2 <- many (letter <|> digit <|> char '_')
    return $ res1 ++ res2
