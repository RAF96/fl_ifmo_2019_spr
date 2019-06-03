module Expression where

import Text.Printf
import Combinators
import Prelude hiding ((<*>))


data Operator = Pow
              | Mul
              | Div
              | Sum
              | Minus
              | Eq
              | Neq
              | Le
              | Lt
              | Ge
              | Gt
              | Conj
              | Disj
              | UnMinus
              | UnNeg

-- Simplest abstract syntax tree for expressions: only binops are allowed
data EAst a = BinOp Operator (EAst a) (EAst a)
            | UnOp Operator (EAst a)
            | Primary a
            | Var String


left_bracket = do
    _ <- char '('
    return ()

right_bracket = do
    _ <- char ')'
    return ()

gapes = do
    _ <- many_gapes
    return ()

between_gapes x = gapes *> x <*gapes

expression' :: [EAstOps Char String a] -> Parser Char a -> Parser Char a
expression' = getExpressionWithShortConstructor left_bracket right_bracket gapes


listOfOperatorsForEAst =
    [
      binToEAstOps (RAssoc, [ (between_gapes $ string "||", BinOp Disj)]),
      binToEAstOps (RAssoc, [ (between_gapes $ string "&&", BinOp Conj)]),
      binToEAstOps (RAssoc, [ (between_gapes $ string "^", BinOp Pow) ]),
      binToEAstOps (NAssoc, [ (between_gapes $ string "==", BinOp Eq),
                 (between_gapes $ string "!=", BinOp Neq),
                 (between_gapes $ string "<=", BinOp Le),
                 (between_gapes $ string ">=", BinOp Ge),
                 (between_gapes $ string "<", BinOp Lt),
                 (between_gapes $ string ">", BinOp Gt)
               ]),
      binToEAstOps (LAssoc, [ (between_gapes $ string "+", BinOp Sum),
                 (between_gapes $ string "-", BinOp Minus)
               ]),
      binToEAstOps (LAssoc, [ (between_gapes $ string "*", BinOp Mul),
                 (between_gapes $ string "/", BinOp Div)
               ]),
      binToEAstOps (RAssoc, [ (between_gapes $ string "^", BinOp Pow) ]),
      unToEAstOps [ (between_gapes $string "!", UnOp UnNeg),
                    (between_gapes $string "-", UnOp UnMinus)]
    ]


primaryForEAst = between_gapes $ (pDigit <|> pVar)

-- Constructs AST for the input expression
parseExpression :: String -> Either ParseError (EAst Integer)
parseExpression input =
  runParserUntilEof ((expression' listOfOperatorsForEAst primaryForEAst) <* (end_of_line <|> eof)) input

-- Change the signature if necessary
-- Calculates the value of the input expression
executeExpression :: String -> Either ParseError Integer
executeExpression input =
  runParserUntilEof ((expression' listOfOperatorsForInteger primaryForInteger) <* (end_of_line <|> eof)) input

listOfOperatorsForInteger :: [EAstOps Char String Integer]
listOfOperatorsForInteger =
    [
      binToEAstOps (RAssoc, [ (between_gapes $ string "||", \x y -> if x + y > 0 then 1 else 0)]),
      binToEAstOps (RAssoc, [ (between_gapes $ string "&&", \x y -> if x * y > 0 then 1 else 0)]),
      binToEAstOps (NAssoc, [ (between_gapes $ string "==", \x y -> if x == y then 1 else 0),
                 (between_gapes $ string "!=", \x y -> if x == y then 0 else 1),
                 (between_gapes $ string "<=", \x y -> if x <= y then 1 else 0),
                 (between_gapes $ string ">=", \x y -> if x >= y then 1 else 0),
                 (between_gapes $ string "<", \x y -> if x < y then 1 else 0),
                 (between_gapes $ string ">", \x y -> if x > y then 1 else 0)
               ]),
      binToEAstOps (LAssoc, [ (between_gapes $ string "+", (+)),
                 (between_gapes $ string "-", (-))
               ]),
      binToEAstOps (LAssoc, [ (between_gapes $ string "*", (*)),
                 (between_gapes $ string "/", div)
               ]),
      binToEAstOps (RAssoc, [ (between_gapes $ string "^", (^)) ]),
      unToEAstOps [ (between_gapes $ string "!", \x -> if x == 0 then 1 else 0),
                    (between_gapes $ string "-", \x -> -x)]
    ]

primaryForInteger = between_gapes digitsInt

pDigit :: Parser Char (EAst Integer)
pDigit = Primary <$> digitsInt

pVar :: Parser Char (EAst Integer)
pVar = Var <$> do
    x <- char '_' <|> alpha
    y <- many (alpha <|> digit <|> (char '_'))
    return $ x : y

instance Show Operator where
  show Pow   = "^"
  show Mul   = "*"
  show Div   = "/"
  show Sum   = "+"
  show Minus = "-"
  show Eq    = "=="
  show Neq   = "/="
  show Le    = "<="
  show Lt    = "<"
  show Ge    = ">="
  show Gt    = ">"
  show Conj  = "&&"
  show Disj  = "||"
  show UnMinus  = "-"
  show UnNeg  = "!"

instance Show a => Show (EAst a) where
  show = show' 0
    where
      show' n t =
        (if n > 0 then printf "%s|_%s" (concat (replicate (n - 1) "| ")) else id)
        (case t of
                  BinOp op l r -> printf "%s\n%s\n%s" (show op) (show' (ident n) l) (show' (ident n) r)
                  UnOp op x -> printf "%s\n%s" (show op) (show' (ident n) x)
                  Var x -> x
                  Primary x -> show x)
      ident = (+1)

{-
show (BinOp Conj (BinOp Pow (Primary 1) (BinOp Sum (Primary 2) (Primary 3))) (Primary 4))

&&
|_^
| |_1
| |_+
| | |_2
| | |_3
|_4
-}

