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

# Desired API

It is my intention that this is as simple as possible and gets directly to
what the user wants to do when using a json parser - extract information from
json of known structure.

JSON Arrays are implemented as a list of values.

JSON Objects are implemented as a list of key/value pairs.

Everything else is totally obvious (although I'd like to think the above two are obvious too).

## Function

extract :: [JSONOperation] -> String -> Maybe Value

  Applies the series of operations on the json to narrow down to the
  information you want.

## Types

### Value:

AValue [Value] (arrays)

OValue [(String, Value)] (objects)

SValue String (strings)

NValue String (numbers)

BValue Bool (bools)

NullValue (null)

## Usage

Gimme a few days ok
