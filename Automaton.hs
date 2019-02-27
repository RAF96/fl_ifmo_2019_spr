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
parseAutomaton input = case runParser foo input of
    Nothing -> Nothing
    Just (str, value) -> Just value


bar x = case x of 
    (v1:v2:v3:l) -> ((v1,v2), Just v3)

foo :: Parser String (Automaton String String)
foo = do
        sigma' <- parseSigma 
        let sigma = Set.fromList sigma'
        states'  <- parseStates 
        let states = Set.fromList states'
        initState'  <- parseInitState 
        let initState = head initState'
        termState'  <- parseTermState
        let termState = Set.fromList termState'
        delta' <- parseDelta 
        let delta'' = bar <$> delta'
        let delta = Map.fromList delta'' 
        return $ Automaton sigma states initState termState delta


parseList :: Parser String elem -> Parser String a -> 
             Parser String b -> Parser String c -> Int -> Parser String [elem]
parseList elem delim lbr rbr minimumNumberElems = do
    lbr
    many_spaces
    head' <- many (elem <* many_spaces <*
                  delim <* many_spaces)
    last_element <- elem
    (many_spaces <* rbr)
    if length head' + 1 > minimumNumberElems then
        return $ head' ++ [last_element]
    else
        fail 

lbr = char '<'
rbr = char '>'
delim = char ','

parseSigma = parseList (some letter) delim lbr rbr 0
parseStates = parseList (some letter) delim lbr rbr 0
parseInitState = parseList (some letter) delim lbr rbr 0
parseTermState = parseList (some letter) delim lbr rbr 0

parseOneDelta = parseList (some letter) delim (char '(') (char ')') 0
parseDelta = parseList (parseOneDelta) delim lbr rbr 0

