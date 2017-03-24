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
  makeTest "reduces singleton list of key chars"
      (reduce [KeyChar "x"])
      [Key "x"]
        ]