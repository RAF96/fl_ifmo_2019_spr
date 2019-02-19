{-# LANGUAGE FlexibleContexts #-}

module Tokenizer where

import Text.Parsec
import Data.Either
import Data.Char
import Control.Applicative (some)


data Token = Ident String
           | KeyWord String
           | NumberInt Int  -- Change Number type if you work with something other than Int
           | NumberOther String  -- Change Number type if you work with something other than Int
           deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize input = fromRight [] $ parse (many getToken) "" input 

many1_spaces :: Stream s m Char => ParsecT s u m ()
many1_spaces = do 
    res <- many1 space
    return ()

getToken :: Stream s m Char => ParsecT s u m Token
getToken = ( 
            try (parseKeyWord) <|>
            try (parseBinInteger) <|>
            try (parseInteger) <|>
            try (parseIdent)
            ) <* (many1_spaces <|> eof)


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

getInt :: Char -> Int 
getInt x = Data.Char.digitToInt x

getInteger :: String -> Int
getInteger = foldl (\y x -> y * 10 + (getInt x)) 0



parseInteger :: Stream s m Char => ParsecT s u m Token
parseInteger = NumberInt <$> (getInteger <$> (many1 digit))


parseBinInteger :: Stream s m Char => ParsecT s u m Token
parseBinInteger = NumberOther <$> do
    res1 <- (string "0")
    res2 <- (string "b" <|> string "B")
    res3 <- many (char '0' <|> char '1' <|> char '_')
    return $ res1 ++ res2 ++ res3


parseIdent :: Stream s m Char => ParsecT s u m Token
parseIdent = Ident <$> do
    res1 <- (string "_" <|> ((\x -> [x]) <$> letter))
    res2 <- many (letter <|> digit <|> char '_')
    return $ res1 ++ res2
