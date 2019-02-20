module Combinators where

import qualified Prelude
import Prelude hiding (seq, fail, fmap, (<*>), (>>=))


-- Parsing result is some payload and a suffix of the input which is yet to be parsed
newtype Parser str ok = Parser { runParser :: str -> Maybe (str, ok) }

-- Parser which always succeedes consuming no input
success :: ok -> Parser str ok
success ok = Parser $ \s -> Just (s, ok)

-- Parser which fails no mater the input
fail :: Parser str ok
fail = Parser $ const Nothing

-- Biased choice: if the first parser succeedes, the second is never run
(<|>) :: Parser str ok -> Parser str ok -> Parser str ok
p <|> q = Parser $ \s ->
  case runParser p s of
    Nothing -> runParser q s
    x -> x

-- Default sequence combinator
-- If the first parser succeedes then the second parser is used
-- If the first does not succeed then the second one is never tried
-- The result is collected into a pair
seq :: Parser str a -> Parser str b -> Parser str (a, b)
p `seq` q = Parser $ \s -> 
    case runParser p s of
        Nothing -> Nothing  
        Just (str1, ok1) -> case runParser q str1 of
            Nothing -> Nothing
            Just (str2, ok2) -> Just (str2, (ok1, ok2))

-- Monadic sequence combinator
(>>=) :: Parser str a -> (a -> Parser str b) -> Parser str b
p >>= q = Parser $ \s -> 
    case runParser p s of 
        Nothing -> Nothing 
        Just (str1, ok1) -> runParser (q ok1) str1


-- Applicative sequence combinator
(<*>) :: Parser str (a -> b) -> Parser str a -> Parser str b
p <*> q = Parser $ \s ->
    case runParser p s of 
        Nothing -> Nothing
        Just (str1, ok1) -> case runParser q str1 of
            Nothing -> Nothing
            Just (str2, ok2) -> Just(str2, ok1 ok2) 

-- Applies a function to the parsing result, if parser succeedes
fmap :: (a -> b) -> Parser str a -> Parser str b
fmap f p = Parser $ \s ->
  case runParser p s of
    Just (s', a) -> Just (s', f a)
    _ -> Nothing

-- Applies a parser once or more times
some :: Parser str a -> Parser str [a]
some p = ((:) <$> p) <*> many p 


-- Applies a parser zero or more times
many :: Parser str a -> Parser str [a]
many p = some p <|> pure []



newtype Tree a = Tree (Maybe (Bool, [(a, Tree a)])) deriving Show

add_word :: Eq a => Tree a -> [a] -> Tree a
add_word _ [] = Tree $ Just (True, [])
add_word root (x:xs) = case root of
    Tree Nothing -> Tree $ Just (False, [(x, add_word (Tree Nothing) xs)])
    Tree (Just (terminal, list)) -> case lookup x list of
        Nothing -> Tree $ Just (terminal, (:) (x, add_word (Tree Nothing) xs) list)
        Just tree -> Tree $ Just (terminal, foo <$> list)
            where  
                foo (a, tree) = 
                    if a == x then (a, add_word tree xs)
                    else (a, tree)


-- get :: Eq a => Tree a -> a -> Maybe (Tree a)
get (Tree Nothing) _ = Nothing
get (Tree (Just (terminal, list))) x = lookup x list
    

-- Parses keywords 
keywords :: [String] -> Parser String String
keywords kws = Parser $ \s -> containsInTree root s ""
    where
        root = foldr (\word acc -> add_word acc word) (Tree Nothing) kws

        containsInTree :: Tree Char -> String -> String -> Maybe (String, String)
        containsInTree (Tree Nothing) _ _ = Nothing 
        containsInTree (Tree (Just (False, list))) (' ': xs) acc = Nothing 
        containsInTree (Tree (Just (True, list))) tail'@(' ': xs) acc = Just $ (tail', acc)
        containsInTree (Tree (Just (False, list))) [] acc = Nothing 
        containsInTree (Tree (Just (True, list))) [] acc = Just $ ([], acc)

        containsInTree tree (x : xs) acc = case get tree x of
            Nothing -> Nothing
            Just node -> containsInTree node xs (acc ++ [x])


-- Checks if the first element of the input is the given token
token :: Eq token => token -> Parser [token] token
token t = Parser $ \s ->
  case s of
    (t' : s') | t == t' -> Just (s', t)
    _ -> Nothing

-- Checks if the first character of the string is the one given
char :: Char -> Parser String Char
char = token

space = char ' '

digit = foldr (\x acc -> acc <|> char x) (char '0') "0123456789"

letter = foldr (\x acc -> acc <|> char x) (char 'a') (['a'..'z'] ++ ['A' .. 'Z'])

string :: Eq token => [token] -> Parser [token] [token]
string [] =  success []
string (x:xs) = Parser $ \s ->
  case s of
    (x' : xs') | x == x' -> case runParser (string xs) xs' of
        Nothing -> Nothing
        Just (str, ok) -> Just (str, x:ok)
    _ -> Nothing


instance Functor (Parser str) where 
    fmap = fmap


instance Applicative (Parser str) where
    pure = return 
    (<*>) = (<*>)


instance Monad (Parser str) where
    return = success
    (>>=) = (>>=) 
