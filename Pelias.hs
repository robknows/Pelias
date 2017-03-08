module Pelias where

type JSON = String

parse :: String -> JSON
parse json = json

extract :: String -> JSON -> Maybe String
extract key json = Just "value"
