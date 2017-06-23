module EvalTest where

import Pelias
import RobUnit

main :: IO ()
main = do
  putStrLn "=========EVALUATION========="
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
      (OValue [("key", AValue [SValue "val", NValue "12", BValue True, BValue False, NullValue])]),
  makeTest "can evaluate a 2D array with one empty element"
      (evaluate [LSquare, LSquare, RSquare, RSquare])
      (AValue [AValue []]),
  makeTest "can evaluate a 2D array with one non-empty element"
      (evaluate [LSquare, LSquare, Number "7", RSquare, RSquare])
      (AValue [AValue [NValue "7"]]),
  makeTest "can evaluate a 2D array with two elements, where the first is non-empty"
      (evaluate [LSquare, LSquare, Number "7", RSquare, LSquare, RSquare, RSquare])
      (AValue [AValue [NValue "7"], AValue []]),
  makeTest "can evaluate a 2D array with two elements, where the second is non-empty"
      (evaluate [LSquare, LSquare, RSquare, LSquare, Number "7", RSquare, RSquare])
      (AValue [AValue [], AValue [NValue "7"]]),
  makeTest "can evaluate a 2D array with two elements, where the neither are non-empty"
      (evaluate [LSquare, LSquare, Number "-18", RSquare, LSquare, Number "7", RSquare, RSquare])
      (AValue [AValue [NValue "-18"], AValue [NValue "7"]]),
  makeTest "can evaluate an array containing a single empty object"
      (evaluate [LSquare, LCurly, RCurly, RSquare])
      (AValue [OValue []]),
  makeTest "can evaluate an array containing a single non-empty object"
      (evaluate [LSquare, LCurly, Pair ("k", [StringValue "v"]), RCurly, RSquare])
      (AValue [OValue [("k", SValue "v")]]),
  makeTest "can evaluate an array containing two objects, where the first is non-empty"
      (evaluate [LSquare, LCurly, Pair ("k", [StringValue "v"]), RCurly, LCurly, RCurly, RSquare])
      (AValue [OValue [("k", SValue "v")], OValue []]),
  makeTest "can evaluate an array containing two objects, where the second is non-empty"
      (evaluate [LSquare, LCurly, RCurly, LCurly, Pair ("k", [StringValue "v"]), RCurly, RSquare])
      (AValue [OValue [], OValue [("k", SValue "v")]]),
  makeTest "can evaluate an array containing two objects, where the neither are non-empty"
      (evaluate [LSquare, LCurly, Pair ("bool", [Const T]), RCurly, LCurly, Pair ("k", [StringValue "v"]), RCurly, RSquare])
      (AValue [OValue [("bool", BValue True)], OValue [("k", SValue "v")]]),
  makeTest "can evaluate an array containing two objects, where the neither are non-empty and one contains an array"
      (evaluate [LSquare, LCurly, Pair ("arr", [LSquare, Const T, RSquare]), RCurly, LCurly, Pair ("k", [StringValue "v"]), RCurly, RSquare])
      (AValue [OValue [("arr", AValue [BValue True])], OValue [("k", SValue "v")]]),
  makeTest "can evaluate an array containing two objects, where the neither are non-empty and one contains a nested object"
      (evaluate [LSquare, LCurly, Pair ("arr", [LSquare, Const T, RSquare]), RCurly, LCurly, Pair ("k", [LCurly, Pair ("x", [StringValue "v"]), RCurly]), RCurly, RSquare])
      (AValue [OValue [("arr", AValue [BValue True])], OValue [("k", OValue [("x", SValue "v")])]])
  
        ]
