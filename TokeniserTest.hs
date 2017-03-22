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
  
  -- Normal (Value) String
  makeTest "tokenising empty string"
    (tokens JValueString "\"\"")
    [Quote, Quote],
  makeTest "tokenising easy string"
    (tokens JValueString "\"hi\"")
    [Quote, ValueChar, ValueChar, Quote],
  makeTest "tokenising string with quote inside it"
    (tokens JValueString "\"yo\\\"yo\\\"\"")
    [Quote, ValueChar, ValueChar, ValueChar, ValueChar, ValueChar, ValueChar, Quote],
  makeTest "tokenising string with many quotes inside it"
    (tokens JValueString "\"a\\\"b\\\"cd\"")
    [Quote, ValueChar, ValueChar, ValueChar, ValueChar, ValueChar, ValueChar, Quote],
  makeTest "tokenising string with a hex literal"
    (tokens JValueString "\"HEX:\\ua32f\"")
    [Quote, ValueChar, ValueChar, ValueChar, ValueChar, ValueChar, Quote],
  makeTest "tokenising string with escaped things"
    (tokens JValueString "\"ayy\\b\\rlm\\f\\nao\\t\"")
    ([Quote] ++ (take 12 (repeat ValueChar)) ++ [Quote]),
  
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
    [Quote, KeyChar, KeyChar, KeyChar, KeyChar, KeyChar, Quote, Colon, Digit, Digit, Digit, Exp LEP, Digit, Digit, Digit],
  makeTest "tokenising a pair whose value is a bool"
    (tokens JPair "\"admin\":false")
    [Quote, KeyChar, KeyChar, KeyChar, KeyChar, KeyChar, Quote, Colon, F],
  makeTest "tokenising a pair whose value is an empty array"
    (tokens JPair "\"willsPals\":[]")
    [Quote, KeyChar, KeyChar, KeyChar, KeyChar, KeyChar, KeyChar, KeyChar, KeyChar, KeyChar, Quote, Colon, LSquare, RSquare],
  makeTest "tokenising a pair whose value is a string"
    (tokens JPair "\"ayy\":\"lmao\"")
    [Quote, KeyChar, KeyChar, KeyChar, Quote, Colon, Quote, ValueChar, ValueChar, ValueChar, ValueChar, Quote],
  
  
  -- Object
  makeTest "tokenising an empty object"
    (tokens JObject "{}")
    [LCurly, RCurly],
  makeTest "tokenising an object with one pair"
    (tokens JObject "{\"x\":\"d\"}")
    [LCurly, Quote, KeyChar, Quote, Colon, Quote, ValueChar, Quote, RCurly],
  makeTest "tokenising an object with a few pairs of different types"
    (tokens JObject "{\"x\":22.5E-7,\"name\":\"buddha\",\"stuff\":[],\"t\":true}")
    [LCurly, Quote, KeyChar, Quote, Colon, Digit, Digit, Dot, Digit, Exp EM, Digit, Comma, Quote, KeyChar, KeyChar, KeyChar, KeyChar, Quote, Colon, Quote, ValueChar, ValueChar, ValueChar, ValueChar, ValueChar, ValueChar, Quote, Comma, Quote, KeyChar, KeyChar, KeyChar, KeyChar, KeyChar, Quote, Colon, LSquare, RSquare, Comma, Quote, KeyChar, Quote, Colon, T, RCurly],
  makeTest "tokenising a  big object with lots of whitespace and multiple layers of nesting"
    (tokens JObject "{\"menu\":{\"id\" : \"file\",\n\t\"value\" : \"File\",\n\t\"popup\" : {\"menuitem\" : [1 , 2 ,3, 4]}\n\t}\n}")
    [LCurly,Quote,KeyChar,KeyChar,KeyChar,KeyChar,Quote,Colon,LCurly,Quote,KeyChar,KeyChar,Quote,Colon,Quote,ValueChar,ValueChar,ValueChar,ValueChar,Quote,Comma,Quote,KeyChar,KeyChar,KeyChar,KeyChar,KeyChar,Quote,Colon,Quote,ValueChar,ValueChar,ValueChar,ValueChar,Quote,Comma,Quote,KeyChar,KeyChar,KeyChar,KeyChar,KeyChar,Quote,Colon,LCurly,Quote,KeyChar,KeyChar,KeyChar,KeyChar,KeyChar,KeyChar,KeyChar,KeyChar,Quote,Colon,LSquare,Digit,Comma,Digit,Comma,Digit,Comma,Digit,RSquare,RCurly,RCurly,RCurly],
  
  -- Array
  makeTest "tokenising an empty array"
    (tokens JArray "[]")
    [LSquare, RSquare],
  makeTest "tokenising an array with one thing"
    (tokens JArray "[\"x\"]")
    [LSquare, Quote, ValueChar, Quote, RSquare],
  makeTest "tokenising an array with a few things"
    (tokens JArray "[{\"key\":true,\"code\":0099}, null, {}]")
    [LSquare, LCurly, Quote, KeyChar, KeyChar, KeyChar, Quote, Colon, T, Comma, Quote, KeyChar, KeyChar, KeyChar, KeyChar, Quote, Colon, Digit, Digit, Digit, Digit, RCurly, Comma, N, Comma, LCurly, RCurly, RSquare]
  
        ]
