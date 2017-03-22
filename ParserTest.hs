module ParserTest where

import Tokeniser
import Parser
import RobUnit

-- Todo: Use quickcheck

main :: IO ()
main = do
  putStrLn "=========RUNNING TESTS========="
  (putStr . concat . appendFailedCount) tests
  putStrLn "ALL DONE"

tests :: [String]
tests = [
  makeTest "parsing empty list of tokens"
    (parse [])
    (Leaf "ROOT")
        ]
