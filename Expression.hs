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

pEq = do
    x <- pPlus
    many_spaces
    op <- pEq''
    many_spaces
    y <- pPlus
    return $ BinOp op x y

pEq'' = pEq' <|> pNeq <|> pLe <|> pLe <|> pLt <|> pGe <|> pGt
pEq' = string "=="  *> pure Eq
pNeq = string "!=" *> pure Neq
pLe = string  "<=" *> pure Le
pLt = string "<" *> pure Lt
pGe = string ">=" *> pure Ge
pGt = string ">" *> pure Gt


pPlus = do
    x <- pMul
    y <- many $ Prelude.fmap flip (BinOp <$> (many_spaces *> pPlus'')) <*> (many_spaces *> pMul)
    let res = foldl (\acc x -> x acc) x y
    return $ res

pPlus'' = pPlus' <|> pMinus
pPlus' = string "+" *> pure Sum
pMinus = string "-" *> pure Minus


pMul = do
    x <- pPow
    y <- many $ Prelude.fmap flip (BinOp <$> (many_spaces *> pMul'')) <*> (many_spaces *> pPow)
    let res = foldl (\acc x -> x acc) x y
    return $ res


pMul'' = pMul' <|> pDiv
pMul' = string "*" *> pure Mul
pDiv = string "/" *> pure Div


pPow = do
    x <- pExp
    many_spaces
    op <- string "^"
    many_spaces
    y <- pPow
    return $ BinOp Conj x y
    <|> pExp




pExp = do
    string "("
    many_spaces
    x <- pStart
    many_spaces
    string ")"
    return x
    <|> pDigit

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
