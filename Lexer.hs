module Lexer where

import Data.Char

data TokenType = Number | Identifier | Op | EOF deriving Show
type Token = (TokenType, String)

lex  :: String -> (Token, String)
lex []                      = ((EOF, []), [])
lex  xs'@(x:xs) | isDigit x = let (val, rest) = scan isDouble  xs [x]
                              in ((Number, val), rest)
                | isAlpha x = let (val, rest) = span isAlphaNum xs'
                              in ((Identifier, val), rest)
                | isSpace x = Lexer.lex xs
                | otherwise = ((Op, [x]), xs)
  where isDouble = (\acc c -> isDigit c || (not $ elem c acc) && c == '.')

scan :: (String -> Char -> Bool) -> String -> String -> (String, String)
scan _ []     acc             = (acc, [])
scan f (x:xs) acc | f acc x   = scan f xs $ acc ++ [x]
                  | otherwise = (acc, (x:xs))
