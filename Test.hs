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
    (extract "key" (Empty :: JSON String))
    Nothing,
  makeTest "can represent a single string field"
    (extract "key" (JSONObject "key" "value"))
    (Just "value"),
  makeTest "can represent a single integer field"
    (extract "key" (JSONObject "key" 3))
    (Just 3)
  ]
