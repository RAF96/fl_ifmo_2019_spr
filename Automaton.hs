module Automaton where

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

import Combinators
import qualified Prelude
import Prelude hiding (seq, fail, fmap, (<*>), (>>=))

type Set = Set.Set
type Map = Map.Map

data Automaton s q = Automaton { sigma     :: Set s
                               , states    :: Set q
                               , initState :: q
                               , termState :: Set q
                               , delta     :: Map (q, s) (Maybe q)
                               } deriving Show

-- Top level function: parses input string, checks that it is an automaton, and then returns it.
-- Should return Nothing, if there is a syntax error or the automaton is not a correct automaton.
-- This includes:
-- * The set of states is empty
-- * The init state is not a state
-- * Any of the terminal states is not a state
-- * Delta function is defined on not-a-state or not-a-symbol-from-sigma
-- Pick appropriate types for s and q

parseAutomaton :: String -> Maybe (Automaton String String)
parseAutomaton input = case runParser parseAutomaton' input of
    Nothing -> Nothing
    Just (str, value) -> Just value


pairFirstTwoElement x = case x of 
    (v1:v2:v3:l) -> ((v1,v2), Just v3)

parseAutomaton' :: Parser String (Automaton String String)
parseAutomaton' = do
        sigma' <- parseSigma 
        let sigma = Set.fromList sigma'
        states'  <- parseStates 
        let states = Set.fromList states'
        initState'  <- parseInitState 
        let initState = head initState'
        termState'  <- parseTermState
        let termState = Set.fromList termState'
        delta' <- parseDelta 
        let delta'' = pairFirstTwoElement <$> delta'
        let delta = Map.fromList delta'' 
        if (null sigma) ||  
           (check2 states [initState]) ||
           (check2 states termState') || 
           (check2 states ((\(a:b:c:l) -> a) <$> delta')) ||
           (check2 sigma ((\(a:b:c:l) -> b) <$> delta'))
        then 
            fail
            --return $ Automaton sigma states initState termState delta
        else
            return $ Automaton sigma states initState termState delta

check2 :: Set String -> [String] -> Bool
check2 first second = foldr (\elem acc -> Set.member elem first || acc) False second


parseList :: Parser String elem -> Parser String a -> 
             Parser String b -> Parser String c -> (Int -> Bool) -> 
             Parser String [elem]
parseList elem delim lbr rbr checkNumberElems = not_empty <|> empty
    where 
        not_empty = do
            lbr
            many_spaces
            head' <- many (elem <* many_spaces <*
                          delim <* many_spaces)
            last_element <- elem
            (many_spaces <* rbr)
            if checkNumberElems (length head' + 1) then
                return $ head' ++ [last_element]
            else
                fail
        empty = do
            lbr
            many_spaces
            rbr
            if checkNumberElems 0 then
                return $ [] 
            else
                fail

lbr = char '<'
rbr = char '>'
delim = char ','

parseOneLetter = (some letter)
parseOneState = (some letter)

parseSigma = parseList (parseOneLetter) delim lbr rbr ((<=) 0)
parseStates = parseList (parseOneState) delim lbr rbr ((<=) 0)
parseInitState = parseList (parseOneState) delim lbr rbr ((==) 1)
parseTermState = parseList (parseOneState) delim lbr rbr ((<=) 0) 

parseOneDelta = parseList (parseOneState) delim (char '(') (char ')') ((==) 3)
parseDelta = parseList (parseOneDelta) delim lbr rbr ((<=) 0) 

