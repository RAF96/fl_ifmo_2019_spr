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

-- Simplest abstract syntax tree for expressions: only binops are allowed
data EAst a = BinOp Operator (EAst a) (EAst a)
            | Primary a


listOfOperatorsForEAst =
    [
      (RAssoc, [ (string "||", BinOp Disj)]),
      (RAssoc, [ (string "&&", BinOp Conj)]),
      (NAssoc, [ (string "==", BinOp Eq),
                 (string "!=", BinOp Neq),
                 (string "<=", BinOp Le),
                 (string ">=", BinOp Ge),
                 (string "<", BinOp Lt),
                 (string ">", BinOp Gt)
               ]),
      (LAssoc, [ (string "+", BinOp Sum),
                 (string "-", BinOp Minus)
               ]),
      (LAssoc, [ (string "*", BinOp Mul),
                 (string "/", BinOp Div)
               ]),
      (RAssoc, [ (string "^", BinOp Pow) ])
    ]

primaryForEAst = many_spaces *> pDigit <* many_spaces

-- Constructs AST for the input expression
parseExpression :: String -> Either ParseError (EAst Integer)
parseExpression input =
  runParserUntilEof ((expression listOfOperatorsForEAst primaryForEAst) <* (end_of_line <|> eof)) input

-- Change the signature if necessary
-- Calculates the value of the input expression
executeExpression :: String -> Either ParseError Integer
executeExpression input =
  runParserUntilEof ((expression listOfOperatorsForInteger primaryForInteger) <* (end_of_line <|> eof)) input

listOfOperatorsForInteger =
    [
      (RAssoc, [ (string "||", \x y -> if x + y > 0 then 1 else 0)]),
      (RAssoc, [ (string "&&", \x y -> if x * y > 0 then 1 else 0)]),
      (NAssoc, [ (string "==", \x y -> if x == y then 1 else 0),
                 (string "!=", \x y -> if x == y then 0 else 1),
                 (string "<=", \x y -> if x <= y then 1 else 0),
                 (string ">=", \x y -> if x >= y then 1 else 0),
                 (string "<", \x y -> if x < y then 1 else 0),
                 (string ">", \x y -> if x > y then 1 else 0)
               ]),
      (LAssoc, [ (string "+", (+)),
                 (string "-", (-))
               ]),
      (LAssoc, [ (string "*", (*)),
                 (string "/", div)
               ]),
      (RAssoc, [ (string "^", (^)) ])
    ]

primaryForInteger = many_spaces *> digitsInt <* many_spaces

pDigit :: Parser Char (EAst Integer)
pDigit = Primary <$> digitsInt

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

instance Show a => Show (EAst a) where
  show = show' 0
    where
      show' n t =
        (if n > 0 then printf "%s|_%s" (concat (replicate (n - 1) "| ")) else id)
        (case t of
                  BinOp op l r -> printf "%s\n%s\n%s" (show op) (show' (ident n) l) (show' (ident n) r)
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
