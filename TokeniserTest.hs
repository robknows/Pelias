module TokeniserTest where

import Tokeniser
import RobUnit

-- Todo: Use quickcheck

main :: IO ()
main = do
  putStrLn "=========RUNNING TESTS========="
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
      [StringValue "abc"]
        ]