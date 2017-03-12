module Test where

import Tokeniser
import RobUnit

main :: IO ()
main = do
  putStrLn "=========RUNNING TESTS========="
  (putStr . concat . appendFailedCount) tests
  putStrLn "ALL DONE"

tests :: [String]
tests = [
  makeTest "tokenising digits"
    (tokens JDigits "0001")
    [Digit, Digit, Digit, Digit]
        ]
