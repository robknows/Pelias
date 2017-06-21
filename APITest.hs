module APITest where

import Parser
import RobUnit

-- Todo: Use quickcheck

main :: IO ()
main = do
  putStrLn "=========API========="
  (putStr . concat . appendFailedCount) tests
  putStrLn "ALL DONE"

tests :: [String]
tests = [

  makeTest "can evaluate object"
      (evaluate [LCurly, Pair ("k", [StringValue "v"]), Comma, Pair ("kee", [Number "420"]), Comma, Pair ("xd", [Const T]), RCurly])
      (OValue [("k", SValue "v"), ("kee", NValue "420"), ("xd", BValue T)])
        
        ]
