module APITest where

import Pelias
import RobUnit

main :: IO ()
main = do
  putStrLn "=========API========="
  (putStr . concat . appendFailedCount) tests
  putStrLn "ALL DONE"

-- Disable optimisations
parser :: [JSONOperation] -> String -> Maybe Value
parser ops = (extractValue ops) . parse . removeNewlinesAndTabs

tests :: [String]
tests = [

  makeTest "extract with no operations just parses the whole thing"
      (parser [] "{\"key\":\"value\"}")
      (Just $ OValue [("key", SValue "value")]),
  makeTest "get a key from an object"
      (parser [Get "key"] "{\"key\":\"value\"}")
      (Just $ SValue "value"),
  makeTest "get a value from an array"
      (parser [Index 0] "[\"key\",\"value\"]")
      (Just $ SValue "key"),
  makeTest "apply a get then an index"
      (parser [Get "key", Index 1] "{\"key\":[\"v0\", \"v1\", \"v2\"]}")
      (Just $ SValue "v1")
  
        ]
