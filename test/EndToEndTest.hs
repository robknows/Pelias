module EndToEndTest where

import Pelias
import RobUnit

main :: IO ()
main = do
  -- File test (Here because it involves IO cancer)
  json            <- readFile "test.json"
  (AValue values) <- pure $ parse json
  len             <- pure $ length values

  putStrLn "=========END TO END========="
  (putStr . concat . appendFailedCount) (tests ++ [makeTest "array of many large objects" len 6])
  putStrLn "ALL DONE"

tests :: [String]
tests = [

  makeTest "empty strings"
      (parse "{\"gravatar_id\": \"\",\"abc\":\"123\"}")
      (OValue [("gravatar_id", SValue ""), ("abc", SValue "123")])
        
        ]
