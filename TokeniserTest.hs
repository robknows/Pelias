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
    [N],
  
  -- Pair
  makeTest "tokenising a pair"
    (tokens JPair "\"memes\":420e+247")
    [Quote, Chr, Chr, Chr, Chr, Chr, Quote, Colon, Digit, Digit, Digit, Exp LEP, Digit, Digit, Digit],
  makeTest "tokenising a pair whose value is a bool"
    (tokens JPair "\"admin\":false")
    [Quote, Chr, Chr, Chr, Chr, Chr, Quote, Colon, F],
  makeTest "tokenising a pair whose value is an empty array"
    (tokens JPair "\"willsPals\":[]")
    [Quote, Chr, Chr, Chr, Chr, Chr, Chr, Chr, Chr, Chr, Quote, Colon, LSquare, RSquare],
    
  -- Object
  makeTest "tokenising an empty object"
    (tokens JObject "{}")
    [LCurly, RCurly],
  makeTest "tokenising an object with one pair"
    (tokens JObject "{\"x\":\"d\"}")
    [LCurly, Quote, Chr, Quote, Colon, Quote, Chr, Quote, RCurly],
  makeTest "tokenising an object with a few pairs of different types"
    (tokens JObject "{\"x\":22.5E-7,\"name\":\"buddha\",\"stuff\":[],\"t\":true}")
    [LCurly, Quote, Chr, Quote, Colon, Digit, Digit, Dot, Digit, Exp EM, Digit, Comma, Quote, Chr, Chr, Chr, Chr, Quote, Colon, Quote, Chr, Chr, Chr, Chr, Chr, Chr, Quote, Comma, Quote, Chr, Chr, Chr, Chr, Chr, Quote, Colon, LSquare, RSquare, Comma, Quote, Chr, Quote, Colon, T, RCurly],
  
  -- Array
  makeTest "tokenising an empty array"
    (tokens JArray "[]")
    [LSquare, RSquare],
  makeTest "tokenising an array with one thing"
    (tokens JArray "[\"x\"]")
    [LSquare, Quote, Chr, Quote, RSquare],
  makeTest "tokenising an array with a few things"
    (tokens JArray "[{\"key\":true,\"code\":0099}, null, {}]")
    [LSquare, LCurly, Quote, Chr, Chr, Chr, Quote, Colon, T, Comma, Quote, Chr, Chr, Chr, Chr, Quote, Colon, Digit, Digit, Digit, Digit, RCurly, Comma, N, Comma, LCurly, RCurly, RSquare]
  
        ]
