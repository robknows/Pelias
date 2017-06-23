module APITest where

import Pelias
import RobUnit

main :: IO ()
main = do
  putStrLn "=========API========="
  (putStr . concat . appendFailedCount) tests
  putStrLn "ALL DONE"

tests :: [String]
tests = [

  makeTest "extract with no operations just parses the whole thing"
      (extract [] "{\"key\":\"value\"}")
      (Just $ OValue [("key", SValue "value")])
        
        ]
