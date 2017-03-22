module Parser where

import Tokeniser

data ParseTree = Leaf String | Node String [ParseTree]
  deriving (Show, Eq)

parse :: [Token] -> ParseTree
parse [] = Leaf "ROOT"
