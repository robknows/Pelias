# Pelias

Intended to be JSON parsing for haskell that works and has an API that is straightforward and clean, unlike Aeson whose API I find pretty stupid.
It is my intention that this works as a single file that you can chuck into your project, import and BAM - easy json parsing.

## Testing

To run the tests, simply run "./run-tests.sh"

## Required haskell libs

```
cabal update
cabal install MissingH
```

## Desired API

parse :: String -> Maybe Value

jsonType :: Value -> JSONType

retrieveString :: Value -> Maybe String

retrieveNumber :: Value -> Maybe Float

retrieveBool   :: Value -> Maybe Bool

castObject :: Value -> Maybe [(String, Value)]

castArray  :: Value -> Maybe [Value]
