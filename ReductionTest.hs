module TokeniserTest where

import Tokeniser
import RobUnit

-- Todo: Use quickcheck

main :: IO ()
main = do
  putStrLn "=========REDUCER========="
  (putStr . concat . appendFailedCount) tests
  putStrLn "ALL DONE"

tests :: [String]
tests = [
  makeTest "reduces an empty list"
      (reduce [])
      [],
  makeTest "reduces single key char"
      (reduce [KeyChar "x"])
      [Key "x"],
  makeTest "reduces list of key chars"
      (reduce [KeyChar "a", KeyChar "b", KeyChar "c"])
      [Key "abc"],
  makeTest "reduces single value char"
      (reduce [ValueChar "a"])
      [StringValue "a"],
  makeTest "reduces value chars"
      (reduce [ValueChar "a", ValueChar "b", ValueChar "c"])
      [StringValue "abc"],
  makeTest "reduces pair"
      (reduce [KeyChar "a", KeyChar "b", Colon, ValueChar "x", ValueChar "y"])
      [Key "ab", StringValue "xy"],
  makeTest "reduces one-valued object"
      (reduce [LCurly, KeyChar "a", KeyChar "b", Colon, ValueChar "x", ValueChar "y", RCurly])
      [LCurly, Key "ab", StringValue "xy", RCurly],
  makeTest "reduces two-valued object"
      (reduce [LCurly, KeyChar "a", KeyChar "b", Colon, ValueChar "x", ValueChar "y", Comma, KeyChar "k", KeyChar "k", Colon, ValueChar "v", ValueChar "v", ValueChar "v", RCurly])
      [LCurly, Key "ab", StringValue "xy", Comma, Key "kk", StringValue "vvv", RCurly],
  makeTest "reduces one digit integer"
      (reduce [Digit "2"])
      [Number "2"],
  makeTest "reduces multi digit integer"
      (reduce [Digit "2", Digit "3"])
      [Number "23"],
  makeTest "reduces pair"
      (reduce [Key "ab", StringValue "xy"])
      [Pair ("ab", StringValue "xy")]
  
       ]
