module Combinators where
-- Комбинаторы все есть кроме eof
-- Есть баги, с парсингом новой строки
--

import qualified Prelude
import Prelude hiding (seq, fail, fmap, (<*>), (>>=))
import Data.Char


data ParseError = GroupError [ParseError] | BaseError String Position deriving Show
type Position = (Int, Int)
data Stream token = Stream {streamData::[token], streamPosition::Position} deriving Show
newtype Parser token ok = Parser { runParserByStream :: Stream token -> Either ParseError (Stream token, ok) }


unexpectedTokenException :: Show token => Position -> token -> ParseError
unexpectedTokenException pos token = BaseError ("Unexpected token: " ++ (show token)) pos

eofException :: Position -> ParseError
eofException = BaseError "Eof exception"

runParser :: Parser token ok -> [token] -> Either ParseError (Stream token, ok)
runParser parser input = runParserByStream parser $ createStream input

createStream :: [token] -> Stream token
createStream tokens = Stream tokens $ (1, 1)

streamNextToken :: Stream token -> Maybe token
streamNextToken stream = case streamData stream of
    h : l -> Just h
    [] -> Nothing

nextLine :: Position -> Position
nextLine (row, column) = (row + 1, 1)

nextLetter :: Position -> Position
nextLetter (row, column) = (row, column + 1)

tailStream :: Stream Char -> Stream Char
tailStream (Stream ('\n' : tail) pos) = Stream tail (nextLine pos)
tailStream (Stream (_ : tail) pos) = Stream tail (nextLetter pos)
tailStream (Stream [] pos) = Stream [] pos


instance Functor (Parser token) where
    fmap = fmap

fmap :: (a -> b) -> Parser token a -> Parser token b
fmap f p = Parser $ \s ->
  case runParserByStream p s of
    Right (s', a) -> Right (s', f a)
    Left x -> Left x

instance Applicative (Parser token) where
    pure = return
    (<*>) = (<*>)

(<*>) :: Parser token (a -> b) -> Parser token a -> Parser token b
p <*> q = Parser $ \s ->
    case runParserByStream p s of
        Left x -> Left x
        Right (str1, ok1) -> case runParserByStream q str1 of
            Left x -> Left x
            Right (str2, ok2) -> Right (str2, ok1 ok2)

instance Monad (Parser token) where
    return = success
    (>>=) = (>>=)
    fail = fail

success :: ok -> Parser token ok
success ok = Parser $ \s -> Right (s, ok)

(>>=) :: Parser token a -> (a -> Parser token b) -> Parser token b
p >>= q = Parser $ \s ->
    case runParserByStream p s of
        Left x -> Left x
        Right (str1, ok1) -> runParserByStream (q ok1) str1

fail :: String -> Parser token ok
fail message = Parser $ \s -> Left $ BaseError message (streamPosition s)

(<|>) :: Parser token ok -> Parser token ok -> Parser token ok
p <|> q = Parser $ \s ->
  case runParserByStream p s of
    Right x -> Right x
    Left x -> case runParserByStream q s of
        Right x -> Right x
        Left y -> Left $ GroupError [x, y]

seq :: Parser token a -> Parser token b -> Parser token (a, b)
p `seq` q = Parser $ \s ->
    case runParserByStream p s of
        Left x -> Left x
        Right (str1, ok1) -> case runParserByStream q str1 of
            Left x -> Left x
            Right (str2, ok2) -> Right (str2, (ok1, ok2))


some :: Parser token a -> Parser token [a]
some p = ((:) <$> p) <*> many p


many :: Parser token a -> Parser token [a]
many p = some p <|> pure []

satisfy :: (Char -> Bool) -> Parser Char Char
satisfy parser = Parser $ \stream -> case streamNextToken stream of
    Just token -> case parser token of
        True  -> Right (tailStream stream, token)
        False -> Left $ unexpectedTokenException (streamPosition stream) token
    Nothing -> Left $ eofException (streamPosition stream)


token :: Char -> Parser Char Char
token letter = satisfy (== letter)

char :: Char -> Parser Char Char
char = token

space :: Parser Char Char
space = char ' '

digit :: Parser Char Char
digit = satisfy (isDigit)

letter :: Parser Char Char
letter = satisfy (isLetter)

string :: String -> Parser Char String
string = foldr (\a b -> pure (:) <*> char a <*> b) (pure [])

many_spaces :: Parser Char ()
many_spaces = do
    res <- many space
    return ()

many1_spaces :: Parser Char ()
many1_spaces = do
    res <- some space
    return ()
