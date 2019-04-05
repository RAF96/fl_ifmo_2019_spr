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

-- Change the signature if necessary
parseExpression :: String -> Either ParseError (Stream Char, EAst Integer)
parseExpression input = runParser pStart input


pStart :: Parser Char (EAst Integer)
pStart = pOr

pOr :: Parser Char (EAst Integer)
pOr = do
    x <- pAnd
    y <- many $ many_spaces *> string "||" *> many_spaces *> pAnd
    let res = foldl (\acc x -> BinOp Disj acc x) x y
    return $ res

pAnd = do
    x <- pEq
    many_spaces
    op <- string "&&"
    many_spaces
    y <- pAnd
    return $ BinOp Conj x y
    <|> pEq

generatePEq sep op = do
    x <- pPlus
    many_spaces
    string sep
    many_spaces
    y <- pPlus
    return $ BinOp op x y

pEq = pEq' <|> pNeq <|> pLe <|> pLe <|> pLt <|> pGe <|> pGt <|> pPlus
pEq' = generatePEq "==" Eq
pNeq = generatePEq "!=" Neq
pLe = generatePEq "<=" Le
pLt = generatePEq "<" Lt
pGe = generatePEq ">=" Ge
pGt = generatePEq ">" Gt


generatePPlus sep op = do
    x <- pMult
    y <- many $ many_spaces *> string sep *> many_spaces *> pMult
    let res = foldl (\acc x -> BinOp op acc x) x y
    return $ res

pPlus = pPlus' <|> pMinus

pPlus' = generatePPlus "+" Sum
pMinus = generatePPlus "-" Minus


pMult = pExp
pExp = pDigit

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
