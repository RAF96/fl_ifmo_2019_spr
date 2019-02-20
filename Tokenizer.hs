{-# LANGUAGE FlexibleContexts #-}

module Tokenizer where

import Combinators 
import Data.Char


data Token = Ident String
           | KeyWord String
           | NumberInt Int  -- Change Number type if you work with something other than Int
           | NumberOther String  -- Change Number type if you work with something other than Int
           deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize input = case runParser (many (getToken <* (many1_spaces <|> eof))) input of
    Nothing -> [] 
    Just x -> snd x

getToken :: Parser String Token 
getToken = ((KeyWord <$> parseKeyWord)  <|>
            (NumberOther <$> parseBinInteger) <|>
            (NumberInt <$> parseInteger) <|>
            (Ident <$> parseIdent)
           )


eof :: Parser [token] ()
eof = Parser $ \s -> case s of
                          [] -> Just ([], ())
                          _  -> Nothing

many1_spaces :: Parser String ()
many1_spaces = do 
    res <- some space
    return ()



parseKeyWord :: Parser String String 
parseKeyWord = keywords keyWords
    where
        keyWords = ["False", "await", "else", "import", "pass",
                "None", "break", "except", "in", "raise",
                "True", "class", "finally", "is", "return",
                "and", "continue",  "for",       "lambda",    "try",
                "as",         "def",        "from",       "nonlocal",   "while",
                "assert",    "del",       "global",    "not", "with",
                "async",      "elif",       "if",         "or",         "yield"]


parseBinInteger :: Parser String String
parseBinInteger = do
    res1 <- (string "0")
    res2 <- (string "b" <|> string "B")
    res3 <- many (char '0' <|> char '1' <|> char '_')
    return $ res1 ++ res2 ++ res3


getInt :: Char -> Int 
getInt x = Data.Char.digitToInt x

getInteger :: String -> Int
getInteger = foldl (\y x -> y * 10 + (getInt x)) 0


parseInteger :: Parser String Int 
parseInteger = (getInteger <$> (some digit))


parseIdent :: Parser String String
parseIdent = do
    res1 <- (string "_" <|> ((\x -> [x]) <$> letter))
    res2 <- many (letter <|> digit <|> char '_')
    return $ res1 ++ res2
