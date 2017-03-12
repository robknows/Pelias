module TokeniserTest where

import Tokeniser
import RobUnit

main :: IO ()
main = do
  putStrLn "=========RUNNING TESTS========="
  (putStr . concat . appendFailedCount) tests
  putStrLn "ALL DONE"

tests :: [String]
tests = [
  -- Digits
  makeTest "tokenising digits"
    (tokens JDigits "0001")
    [Digit, Digit, Digit, Digit],
  makeTest "tokenising one digit"
    (tokens JDigits "5")
    [Digit],
  makeTest "tokenising many digits"
    (tokens JDigits "2384283462837462")
    (take 16 (repeat Digit)),
  
  -- Int
  makeTest "tokenising positive int"
    (tokens JInt "1234")
    [Digit, Digit, Digit, Digit],
  makeTest "tokenising negative int"
    (tokens JInt "-937")
    [Minus, Digit, Digit, Digit],
  
  -- SimpleNumber
  makeTest "tokenising decimal point number"
    (tokens JSimpleNumber "11.454")
    [Digit, Digit, Dot, Digit, Digit, Digit],
  makeTest "tokenising integer"
    (tokens JSimpleNumber "43")
    [Digit, Digit],
  
  -- Exp
  makeTest "tokenising exponent E+"
    (tokens JExp "E+")
    [Exp EP],
  makeTest "tokenising exponent e-"
    (tokens JExp "e-")
    [Exp LEM],
  makeTest "tokenising exponent e"
    (tokens JExp "e")
    [Exp LE],
  
  --Number
  makeTest "tokenising negative fraction"
    (tokens JNumber "-23.44")
    [Minus, Digit, Digit, Dot, Digit, Digit],
  makeTest "tokenising positive number with standard exponent"
    (tokens JNumber "86.6E23")
    [Digit, Digit, Dot, Digit, Exp E, Digit, Digit],
  makeTest "tokenising negative integer with funky exponent"
    (tokens JNumber "-97e-2")
    [Minus, Digit, Digit, Exp LEM, Digit],
  makeTest "tokenising negative fraction with funky exponenet"
    (tokens JNumber "-5.1E+65")
    [Minus, Digit, Dot, Digit, Exp EP, Digit, Digit],
  
  -- String
  makeTest "tokenising empty string"
    (tokens JString "\"\"")
    [Quote, Quote],
  makeTest "tokenising easy string"
    (tokens JString "\"hi\"")
    [Quote, Chr, Chr, Quote],
  makeTest "tokenising string with quote inside it"
    (tokens JString "\"yo\\\"yo\\\"\"")
    [Quote, Chr, Chr, Chr, Chr, Chr, Chr, Quote],
  makeTest "tokenising string with many quotes inside it"
    (tokens JString "\"a\\\"b\\\"cd\"")
    [Quote, Chr, Chr, Chr, Chr, Chr, Chr, Quote],
  makeTest "tokenising string with a hex literal"
    (tokens JString "\"HEX:\\ua32f\"")
    [Quote, Chr, Chr, Chr, Chr, Chr, Quote],
  makeTest "tokenising string with escaped things"
    (tokens JString "\"ayy\\b\\rlm\\f\\nao\\t\"")
    ([Quote] ++ (take 12 (repeat Chr)) ++ [Quote]),
  
  -- Bool
  makeTest "tokenising true"
    (tokens JBool "true")
    [T],
  makeTest "tokenising false"
    (tokens JBool "false")
    [F],
  
  -- Null
  makeTest "tokenising null"
    (tokens JNull "null")
    [N]
        ]
