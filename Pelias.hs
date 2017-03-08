module Pelias where

data JSON keytype = Empty | JSONObject String keytype

extract :: String -> JSON a -> Maybe a
extract key Empty = Nothing
extract key (JSONObject k v) = (Just v)