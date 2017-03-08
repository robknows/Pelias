module Pelias where

data JSON = Empty
          | JSON String String

extract :: String -> JSON -> Maybe String
extract key Empty = Nothing
extract key (JSON k v) = (Just v)