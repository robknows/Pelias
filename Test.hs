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
    -- It doesn't know what value it's trying to extract
    --   (because this is a test, and is hence contrived),
    --   so without having the type specified, the compiler
    --   will get scared and complain.
    (extract "key" (Empty :: JSON String))
    Nothing,
  makeTest "can represent a single string field"
    (extract "key" (JSONObject "key" "value"))
    (Just "value"),
  makeTest "can represent a single integer field"
    (extract "key" (JSONObject "key" 3))
    (Just 3)
  ]
