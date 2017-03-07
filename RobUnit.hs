module RobUnit where

import Data.String.Utils

makeTest :: (Eq a, Show a) => String -> a -> a -> String
makeTest description actual expected =
  if expected == actual
  then ""
  else "FAIL: " ++ description ++ "\n\texp: " ++ show expected ++ "\n\tact: " ++ show actual ++ "\n"

appendFailedCount :: [String] -> [String]
appendFailedCount tests = tests ++ ["PASSED: " ++ show passed ++ " FAILED: " ++ show fails ++ "\n"]
  where
    fails  = length (filter (startswith "FAIL") tests)
    passed = length tests - fails
