module Runtime where

import Parser

execute :: Expr -> Double
execute (Value x)          = x
execute (BinOp op lhs rhs) = case op of
                              Add -> (+) (execute lhs) (execute rhs)
                              Sub -> (-) (execute lhs) (execute rhs)
                              Mul -> (*) (execute lhs) (execute rhs)
                              Div -> (/) (execute lhs) (execute rhs)
