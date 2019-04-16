module Test where

import Expression
import Combinators


test1ListOfOperators =
    [ (LAssoc, [ (char '+', BinOp Sum),
                 (char '-', BinOp Minus)
    ])
    , (RAssoc, [ (char '^', BinOp Pow) ])
    ]
test1SimpleParse = pDigit

test1_east = expression test1ListOfOperators test1SimpleParse
test1_value = executeExpression  "1-2-3"


test2ListOfOperators =
    [ (NAssoc, [ (string "==" , BinOp Eq),
                 (string "!=" , BinOp Neq)
    ]),
      (RAssoc, [ (string "+", BinOp Sum)])
    ]
test2SimpleParse = pDigit

test2_east = expression test2ListOfOperators test2SimpleParse
test2_example = runParser test2_east "1!=2"


test3ListOfOperators =
    [  (RAssoc, [ (string "+", BinOp Sum)])
    ]
test3SimpleParse = pDigit

test3_east = expression test3ListOfOperators test3SimpleParse


