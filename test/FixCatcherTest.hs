module FixCatcherTest where

import Control.Monad

import Pelias
import RobUnit

main :: IO ()
main = do
  -- File test (Here because it involves IO cancer)
  -- json            <- readFile "test.json"
  -- (AValue values) <- pure $ parse json
  -- len             <- pure $ length values

  putStrLn "=========FIX CATCHER========="
  runPureTests tests
  runIOTests ioTests
  putStrLn "ALL DONE"

tests :: [String]
tests = [

  makeTest "empty strings"
      (parse "{\"gravatar_id\": \"\",\"abc\":\"123\"}")
      (OValue [("gravatar_id", SValue ""), ("abc", SValue "123")])
        
        ]

getValues :: Value -> [Value]
getValues (AValue values) = values
getValues _               = []

ioTests :: [IO String]
ioTests = [

  ioTest "can get the number of values of an array"
    (liftM (length . getValues . parse) (readFile "test.json"))
    6

          ]
