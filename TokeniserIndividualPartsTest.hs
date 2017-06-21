module TokeniserTest where

import Tokeniser
import RobUnit

-- Todo: Use quickcheck

main :: IO ()
main = do
  putStrLn "=========TOKENISER INDIVIDUAL PARTS========="
  (putStr . concat . appendFailedCount) tests
  putStrLn "ALL DONE"

tests :: [String]
tests = [
  -- Digits
  makeTest "tokenising digits"
    (snd $ tokenise JDigits "0001")
    [Digit "0",Digit "0",Digit "0",Digit "1"],
  makeTest "tokenising one digit"
    (snd $ tokenise JDigits "5")
    [Digit "5"],
  makeTest "tokenising many digits"
    (snd $ tokenise JDigits "2384283462837462")
    [Digit "2",Digit "3",Digit "8",Digit "4",Digit "2",Digit "8",Digit "3",Digit "4",Digit "6",Digit "2",Digit "8",Digit "3",Digit "7",Digit "4",Digit "6",Digit "2"],
  
  -- Int
  makeTest "tokenising positive int"
    (snd $ tokenise JInt "1234")
    [Digit "1",Digit "2",Digit "3",Digit "4"],
  makeTest "tokenising negative int"
    (snd $ tokenise JInt "-937")
    [Minus,Digit "9",Digit "3",Digit "7"],
  
  -- SimpleNumber
  makeTest "tokenising decimal point number"
    (snd $ tokenise JSimpleNumber "11.454")
    [Digit "1",Digit "1",Dot,Digit "4",Digit "5",Digit "4"],
  makeTest "tokenising integer"
    (snd $ tokenise JSimpleNumber "43")
    [Digit "4",Digit "3"],
  
  -- Exp
  makeTest "tokenising exponent E+"
    (snd $ tokenise JExp "E+")
    [Exp EP],
  makeTest "tokenising exponent e-"
    (snd $ tokenise JExp "e-")
    [Exp LEM],
  makeTest "tokenising exponent e"
    (snd $ tokenise JExp "e")
    [Exp LE],
  
  --Number
  makeTest "tokenising negative fraction"
    (snd $ tokenise JNumber "-23.44")
    [Minus,Digit "2",Digit "3",Dot,Digit "4",Digit "4"],
  makeTest "tokenising positive number with standard exponent"
    (snd $ tokenise JNumber "86.6E23")
    [Digit "8",Digit "6",Dot,Digit "6",Exp E,Digit "2",Digit "3"],
  makeTest "tokenising negative integer with funky exponent"
    (snd $ tokenise JNumber "-97e-2")
    [Minus, Digit "9",Digit "7",Exp LEM,Digit "2"],
  makeTest "tokenising negative fraction with funky exponenet"
    (snd $ tokenise JNumber "-5.1E+65")
    [Minus,Digit "5",Dot,Digit "1",Exp EP,Digit "6",Digit "5"],
  
  -- Normal (Value) String
  makeTest "tokenising empty string"
    (snd $ tokenise JValueString "\"\"")
    [Quote, Quote],
  makeTest "tokenising easy string"
    (snd $ tokenise JValueString "\"hi\"")
    [Quote, ValueChar "h", ValueChar "i", Quote],
  makeTest "tokenising string with quote inside it"
    (snd $ tokenise JValueString "\"yo\\\"yo\\\"\"")
    [Quote,ValueChar "y",ValueChar "o",ValueChar "\\\"",ValueChar "y",ValueChar "o",ValueChar "\\\"",Quote],
  makeTest "tokenising string with many quotes inside it"
    (snd $ tokenise JValueString "\"a\\\"b\\\"cd\"")
    [Quote,ValueChar "a",ValueChar "\\\"",ValueChar "b",ValueChar "\\\"",ValueChar "c",ValueChar "d",Quote],
  makeTest "tokenising string with a hex literal"
    (snd $ tokenise JValueString "\"HEX:\\ua32f\"")
    [Quote,ValueChar "H",ValueChar "E",ValueChar "X",ValueChar ":",ValueChar "\\ua32f",Quote],
  makeTest "tokenising string with escaped things"
    (snd $ tokenise JValueString "\"ayy\\b\\rlm\\f\\nao\\t\"")
    [Quote,ValueChar "a",ValueChar "y",ValueChar "y",ValueChar "\\b",ValueChar "\\r",ValueChar "l",ValueChar "m",ValueChar "\\f",ValueChar "\\n",ValueChar "a",ValueChar "o",ValueChar "\\t",Quote],
  
  -- Bool
  makeTest "tokenising true"
    (snd $ tokenise JBool "true")
    [Const T],
  makeTest "tokenising false"
    (snd $ tokenise JBool "false")
    [Const F],
  
  -- Null
  makeTest "tokenising null"
    (snd $ tokenise JNull "null")
    [Const N],
  
  -- Pair
  makeTest "tokenising a pair"
    (snd $ tokenise JPair "\"memes\":420e+247")
    [Quote,KeyChar "m",KeyChar "e",KeyChar "m",KeyChar "e",KeyChar "s",Quote,Colon,Digit "4",Digit "2",Digit "0",Exp LEP,Digit "2",Digit "4",Digit "7"],
  makeTest "tokenising a pair whose value is a bool"
    (snd $ tokenise JPair "\"admin\":false")
    [Quote, KeyChar "a", KeyChar "d", KeyChar "m", KeyChar "i", KeyChar "n", Quote, Colon, Const F],
  makeTest "tokenising a pair whose value is an empty array"
    (snd $ tokenise JPair "\"willsPals\":[]")
    [Quote,KeyChar "w",KeyChar "i",KeyChar "l",KeyChar "l",KeyChar "s",KeyChar "P",KeyChar "a",KeyChar "l",KeyChar "s",Quote,Colon,LSquare,RSquare],
  makeTest "tokenising a pair whose value is a string"
    (snd $ tokenise JPair "\"ayy\":\"lmao\"")
    [Quote,KeyChar "a",KeyChar "y",KeyChar "y",Quote,Colon,Quote,ValueChar "l",ValueChar "m",ValueChar "a",ValueChar "o",Quote],
  
  -- Object
  makeTest "tokenising an empty object"
    (snd $ tokenise JObject "{}")
    [LCurly, RCurly],
  makeTest "tokenising an object with one pair"
    (snd $ tokenise JObject "{\"x\":\"d\"}")
    [LCurly, Quote, KeyChar "x", Quote, Colon, Quote, ValueChar "d", Quote, RCurly],
  makeTest "tokenising an object with a few pairs of different types"
    (snd $ tokenise JObject "{\"x\":22.5E-7,\"name\":\"buddha\",\"stuff\":[],\"t\":true}")
    [LCurly,Quote,KeyChar "x",Quote,Colon,Digit "2",Digit "2",Dot,Digit "5",Exp EM,Digit "7",Comma,Quote,KeyChar "n",KeyChar "a",KeyChar "m",KeyChar "e",Quote,Colon,Quote,ValueChar "b",ValueChar "u",ValueChar "d",ValueChar "d",ValueChar "h",ValueChar "a",Quote,Comma,Quote,KeyChar "s",KeyChar "t",KeyChar "u",KeyChar "f",KeyChar "f",Quote,Colon,LSquare,RSquare,Comma,Quote,KeyChar "t",Quote,Colon,Const T,RCurly],
  makeTest "tokenising a  big object with lots of whitespace and multiple layers of nesting"
    (snd $ tokenise JObject "{\"menu\":{\"id\" : \"file\",\n\t\"value\" : \"File\",\n\t\"popup\" : {\"menuitem\" : [1 , 2 ,3, 4]}\n\t}\n}")
    [LCurly,Quote,KeyChar "m",KeyChar "e",KeyChar "n",KeyChar "u",Quote,Colon,LCurly,Quote,KeyChar "i",KeyChar "d",Quote,Colon,Quote,ValueChar "f",ValueChar "i",ValueChar "l",ValueChar "e",Quote,Comma,Quote,KeyChar "v",KeyChar "a",KeyChar "l",KeyChar "u",KeyChar "e",Quote,Colon,Quote,ValueChar "F",ValueChar "i",ValueChar "l",ValueChar "e",Quote,Comma,Quote,KeyChar "p",KeyChar "o",KeyChar "p",KeyChar "u",KeyChar "p",Quote,Colon,LCurly,Quote,KeyChar "m",KeyChar "e",KeyChar "n",KeyChar "u",KeyChar "i",KeyChar "t",KeyChar "e",KeyChar "m",Quote,Colon,LSquare,Digit "1",Comma,Digit "2",Comma,Digit "3",Comma,Digit "4",RSquare,RCurly,RCurly,RCurly],
  
  -- Array
  makeTest "tokenising an empty array"
    (snd $ tokenise JArray "[]")
    [LSquare, RSquare],
  makeTest "tokenising an array with one thing"
    (snd $ tokenise JArray "[\"x\"]")
    [LSquare, Quote, ValueChar "x", Quote, RSquare],
  makeTest "tokenising an array with a few things"
    (snd $ tokenise JArray "[{\"key\":true,\"code\":0099}, null, {}]")
    [LSquare, LCurly, Quote, KeyChar "k", KeyChar "e", KeyChar "y", Quote, Colon, Const T, Comma, Quote, KeyChar "c", KeyChar "o", KeyChar "d", KeyChar "e", Quote, Colon, Digit "0", Digit "0", Digit "9", Digit "9", RCurly, Comma, Const N, Comma, LCurly, RCurly, RSquare]
  
        ]
