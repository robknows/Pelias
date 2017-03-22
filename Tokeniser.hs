module Tokeniser where

import Data.Char
import Data.String.Utils

-- (E) for the letter 'E'. (L)owercase. (P)lus. (M)inus.
data Exponent = E | EP | EM | LE | LEP | LEM
  deriving (Show, Eq)

data Token = Digit | Minus | Dot | Exp Exponent | KeyChar | ValueChar | Quote | 
             LSquare | RSquare | Comma | LCurly | RCurly | Colon | T | F | N
  deriving (Show, Eq)

-- Used for the tokenise function
data GrammarPart = JDigits | JInt | JSimpleNumber | JExp | JNumber | JKeyString | JValueString |
                   JArray | JElements | JObject | JMembers | JPair | JBool | JNull | JValue

tokens :: GrammarPart -> String -> [Token]
tokens jsonPart = snd . (tokenise jsonPart)

-- The "tokenise" functions, take the String, tokenise what they are meant to
--   and return the remainder of the input and the tokens they gleaned.

-- This is so I don't have to litter my code with strip. Although as I have now
--   learnt, I have had to include some anyway.
tokenise :: GrammarPart -> String -> (String, [Token])
tokenise JDigits       = tokeniseDigits . strip
tokenise JInt          = tokeniseInt . strip
tokenise JSimpleNumber = tokeniseSimpleNumber . strip
tokenise JExp          = tokeniseExp . strip
tokenise JNumber       = tokeniseNumber . strip
tokenise JKeyString    = (tokeniseString KeyChar) . strip
tokenise JValueString  = (tokeniseString ValueChar) . strip
tokenise JArray        = tokeniseArray . strip
tokenise JElements     = tokeniseElements . strip
tokenise JObject       = tokeniseObject . strip
tokenise JMembers      = tokeniseMembers . strip
tokenise JPair         = tokenisePair . strip
tokenise JBool         = tokeniseBool . strip
tokenise JNull         = tokeniseNull . strip
tokenise JValue        = tokeniseValue . strip

tokeniseDigits :: String -> (String, [Token])
tokeniseDigits input = (drop nDigits input, take nDigits (repeat Digit))
  where nDigits = length (takeWhile isDigit input)

tokeniseInt :: String -> (String, [Token])
tokeniseInt ('-' : input) = (noLeadingDigits, Minus : digits)
  where (noLeadingDigits, digits) = tokenise JDigits input
tokeniseInt input = tokenise JDigits input

tokeniseSimpleNumber :: String -> (String, [Token])
tokeniseSimpleNumber input = 
  case noLeadingInt of
    ('.' : digits) -> (noLeadingDecimal, leadingInt ++ [Dot] ++ decimalPart)
    _              -> (noLeadingInt, leadingInt)
  where
    (noLeadingInt, leadingInt)      = tokenise JInt input
    (noLeadingDecimal, decimalPart) = tokenise JDigits (drop 1 noLeadingInt)

tokeniseExp :: String -> (String, [Token])
tokeniseExp ('E' : '+' : rest) = (rest, [Exp EP])
tokeniseExp ('E' : '-' : rest) = (rest, [Exp EM])
tokeniseExp ('E'       : rest) = (rest, [Exp E])
tokeniseExp ('e' : '+' : rest) = (rest, [Exp LEP])
tokeniseExp ('e' : '-' : rest) = (rest, [Exp LEM])
tokeniseExp ('e'       : rest) = (rest, [Exp LE])

tokeniseNumber :: String -> (String, [Token])
tokeniseNumber input =
  if check JExp noLeadingSimpleNum
  then (noNumber, leadingSimpleNum ++ exp ++ expDigits)
  else (noLeadingSimpleNum, leadingSimpleNum)
  where
    (noLeadingSimpleNum, leadingSimpleNum) = tokenise JSimpleNumber input
    (noExp, exp)                           = tokenise JExp noLeadingSimpleNum
    (noNumber, expDigits)                  = tokenise JDigits noExp

tokeniseString :: Token -> String -> (String, [Token])
tokeniseString _ ('\"' : '\"' : rest) = (rest, Quote : [Quote])
tokeniseString t ('\"' : rest)        = tokeniseChars' t [] (strip rest)
tokeniseString t something            = error ("Confused by " ++ something ++ "\n")
-- String parsing blackbox...
tokeniseChars' :: Token -> [Token] -> String -> (String, [Token])
tokeniseChars' _ acc ('\"' : rest)       = (rest, [Quote] ++ acc ++ [Quote])
tokeniseChars' t acc ('\\' : 'u' : rest) =
  if all isHexDigit (take 4 rest)
  then tokeniseChars' t (acc ++ [t]) (drop 4 rest) 
  else error "String with bad usage of \\u - expected 4 hex digits - tokenising failed"
tokeniseChars' t acc ('\\' : c : rest)   =
  if elem c "\"\\/bfnrt"
  then tokeniseChars' t (acc ++ [t]) rest
  else error "String with bad backslash usage - tokenising failed"
tokeniseChars' t acc (c : rest)          = tokeniseChars' t (acc ++ [t]) rest
tokeniseChars' _ acc []                  = ([], acc)

tokeniseArray :: String -> (String, [Token])
tokeniseArray ('[' : ']' : rest) = (rest, LSquare : [RSquare])
tokeniseArray input = ((drop 1 rest), [LSquare] ++ tokenised ++ [RSquare])
  where (rest, tokenised) = tokenise JElements (drop 1 input)

tokeniseElements :: String -> (String, [Token])
tokeniseElements input =
  case strip noValue of
    (',' : rest) -> (noElements, value ++ [Comma] ++ elements)
    _            -> (noValue, value)
  where 
    (noValue, value)       = tokenise JValue input
    (noElements, elements) = tokenise JElements (drop 1 (strip noValue))

tokeniseObject :: String -> (String, [Token])
tokeniseObject ('{' : '}' : rest) = (rest, LCurly : [RCurly])
tokeniseObject input = ((drop 1 (strip rest)), [LCurly] ++ tokenised ++ [RCurly])
  where (rest, tokenised) = tokenise JMembers (drop 1 input)

tokeniseMembers :: String -> (String, [Token])
tokeniseMembers input =
  case noPair of
    (',' : rest) -> (noMembers, pair ++ [Comma] ++ members)
    _            -> (noPair, pair)
  where 
    (noPair, pair)       = tokenise JPair input
    (noMembers, members) = tokenise JMembers (drop 1 noPair)

tokenisePair :: String -> (String, [Token])
tokenisePair input =
  if checkPairWithoutKey noKey
  then (noPair, key ++ [Colon] ++ value)
  else error "Bad pair - tokenising failed"
  where
    (noKey, key)    = tokenise JKeyString input
    (noPair, value) = tokenise JValue (drop 1 (strip noKey))

tokeniseBool :: String -> (String, [Token])
tokeniseBool input
  | take 4 input == "true"  = (drop 4 input, [T])
  | take 5 input == "false" = (drop 5 input, [F])
  | otherwise               = error "Failed to tokenise bool - not true or false"

tokeniseNull :: String -> (String, [Token])
tokeniseNull input
  | take 4 input == "null" = (drop 4 input, [N])
  | otherwise              = error "Failed to tokenise null"

tokeniseValue :: String -> (String, [Token])
tokeniseValue input
  | check JDigits input      = tokenise JNumber input
  | check JValueString input = tokenise JValueString input
  | check JBool   input      = tokenise JBool   input
  | check JNull   input      = tokenise JNull   input
  | check JArray  input      = tokenise JArray  input
  | check JObject input      = tokenise JObject input
  | otherwise                = error ("Tokenising failed, couldn't understand " ++ (take 30 input) ++ "...")

-- The "check" functions return true if it is able to tokenise the next tokens
--   according to the specified member of the grammar.

check :: GrammarPart -> String -> Bool
check JDigits      = checkDigits . strip
check JExp         = checkExp . strip
check JValueString = checkString . strip
check JBool        = checkBool . strip
check JNull        = checkNull . strip
check JArray       = checkArray . strip
check JObject      = checkObject . strip

checkDigits :: String -> Bool
checkDigits = isDigit . head

checkExp :: String -> Bool
checkExp ('e':'+':rest) = checkDigits rest
checkExp ('e':'-':rest) = checkDigits rest
checkExp ('e'    :rest) = checkDigits rest
checkExp ('E':'+':rest) = checkDigits rest
checkExp ('E':'-':rest) = checkDigits rest
checkExp ('E'    :rest) = checkDigits rest
checkExp _              = False

checkString :: String -> Bool
checkString = (== '\"') . head

checkBool :: String -> Bool
checkBool input
  | take 4 input == "true"  = True
  | take 5 input == "false" = True
  | otherwise               = False

checkNull :: String -> Bool
checkNull = (== "null") . (take 4)

checkArray :: String -> Bool
checkArray = (== '[') . head

checkObject :: String -> Bool
checkObject = (== '{') . head

checkPairWithoutKey :: String -> Bool
checkPairWithoutKey = (== ':') . head . strip
