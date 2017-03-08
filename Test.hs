module Test where

import RobUnit
import Pelias

main :: IO ()
main = do
  putStrLn "=========RUNNING TESTS========="
  (putStr . concat . appendFailedCount) tests
  putStrLn "ALL DONE"

tests :: [String]
tests = [
  makeTest "parse the only key from a json object with only one element"
    (extract "key" "{\"key\": \"value\"}")
    (Just "value")
  ]
