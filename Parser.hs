module Parser where

import Lexer

data Operator = Add | Sub | Mul | Div deriving Show
data Expr = Value Double | BinOp Operator Expr Expr deriving Show

parse :: String -> Expr
parse []    = Value 0
parse input = let ((token, val), rest) = Lexer.lex input
              in case token of
                Number -> let ((nextToken, nextVal), nextRest) = Lexer.lex rest
                          in case nextToken of
                            Op -> (BinOp (parseOperator nextVal) (Value (read val)) (parse nextRest))
                            _ -> Value (read val)

parseOperator :: String -> Operator
parseOperator op = case op of
  "+" -> Add
  "-" -> Sub
  "*" -> Mul
  "/" -> Div
