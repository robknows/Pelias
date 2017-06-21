module EndToEndTest where

import Parser
import RobUnit

main :: IO ()
main = do
  putStrLn "=========END TO END========="
  (putStr . concat . appendFailedCount) tests
  putStrLn "ALL DONE"

tests :: [String]
tests = [

  makeTest "empty strings"
      (parse "{\"gravatar_id\": \"\",\"abc\":\"123\"}")
      (OValue [("gravatar_id", SValue ""), ("abc", SValue "123")])
        
        ]
