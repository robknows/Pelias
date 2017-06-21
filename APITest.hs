module APITest where

import Parser
import RobUnit

main :: IO ()
main = do
  putStrLn "=========API========="
  (putStr . concat . appendFailedCount) tests
  putStrLn "ALL DONE"

tests :: [String]
tests = [

  makeTest "can evaluate empty object"
      (evaluate [LCurly,RCurly])
      (OValue []),
  makeTest "can evaluate object with one string value"
      (evaluate [LCurly, Pair ("key", [StringValue "value"]), RCurly])
      (OValue [("key", SValue "value")]),
  makeTest "can evaluate object with two values of different types"
      (evaluate [LCurly, Pair ("key", [StringValue "value"]), Comma, Pair ("num", [Number "15"]), RCurly])
      (OValue [("key", SValue "value"), ("num", NValue "15")]),
  makeTest "can evaluate object with an array value of unit cardinality"
      (evaluate [LCurly, Pair ("key", [LSquare, StringValue "value", RSquare]), RCurly])
      (OValue [("key", AValue [SValue "value"])]),
  makeTest "can evaluate object with an empty array value"
      (evaluate [LCurly, Pair ("key", [LSquare, RSquare]), RCurly])
      (OValue [("key", AValue [])]),
  makeTest "can evaluate object with an array value with many elements of different types"
      (evaluate [LCurly, Pair ("key", [LSquare, StringValue "val", Number "12", Const T, Const F, Const N, RSquare]), RCurly])
      (OValue [("key", AValue [SValue "val", NValue "12", BValue T, BValue F, NullValue])]),
  makeTest "can evaluate a 2D array with one empty element"
      (evaluate [LSquare, LSquare, RSquare, RSquare])
      (AValue [AValue []]),
  makeTest "can evaluate a 2D array with one non-empty element"
      (evaluate [LSquare, LSquare, Number "7", RSquare, RSquare])
      (AValue [AValue [NValue "7"]]),
  makeTest "can evaluate a 2D array with two elements, where the first is non-empty"
      (evaluate [LSquare, LSquare, Number "7", RSquare, LSquare, RSquare, RSquare])
      (AValue [AValue [NValue "7"], AValue []])
 
        ]
