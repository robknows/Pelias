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
  makeTest "can represent empty object"
    (extract "key" Empty)
    Nothing,
  makeTest "can represent a single string field"
    (extract "key" (JSON "key" "value"))
    (Just "value")
  ]
