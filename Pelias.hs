module Pelias where

data JSON a = Empty | JSONObject String a

extract :: String -> JSON a -> Maybe a
extract key Empty = Nothing
extract key (JSONObject k v) = (Just v)