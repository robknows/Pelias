# Pelias

Intended to be JSON parsing for haskell that works and has an API that is straightforward and clean, unlike Aeson whose API I find pretty stupid.
It is my intention that this works as a single file that you can chuck into your project, import and BAM - easy json parsing.

It is very easy to use, but it is slow because I have done no optimisations. It uses the notoriously slow `String` type rather than the much faster `ByteString` and is not mutlithreaded in any way. This is purely on account of my laziness. I had a json related problem (see example) whose solution using Aeson was too shit for me, so I fixed that problem. That's Pelias.

## Installing dependencies

```
./install-dependencies.sh
```

## Testing

To run the tests, simply run `./run-tests.sh`

If you want to play around with an example, there is an example json file and it's optimiser in the folder called "example"

# API

It is my intention that this is as simple as possible and gets directly to
what the user wants to do when using a json parser - extract information from
json of known structure.

JSON Arrays are implemented as a list of values.

JSON Objects are implemented as a list of key/value pairs.

Other values are totally obvious and intuitive

## Types

### Value:

`AValue [Value]` (arrays)

`OValue [(String, Value)]` (objects)

`SValue String` (strings)

`NValue String` (numbers)

`BValue Bool` (bools)

`NullValue` (null)

### JSONOperation

`Get String` (for accessing the field of a JSON object)

`Index Int` (for accessing a given index of a JSON array)

## Usage

Without an optimiser specific to your domain, Pelias is just too slow for any reasonably large json file. I could fix this by changing my implementation to use ByteString, or using concurrency... but I've already made the optimiser for my use case, and it's not very hard anyway.

If your json is small, then you'll be fine. In this case, you can use plain old, unoptimised Pelias, by using the `extract` function:

`extract :: [JSONOperation] -> String -> Maybe Value`

You give a list of operations you want to apply and a string of raw json to apply them to, and it applies them and returns you a result, given everything went well.

`extract` uses the default optimiser aka no optimiser at all. Hence it is very slow.

If you intend to do things at a reasonable speed, you will need to write an optimiser. An optimiser in Pelias is simply a function that takes a big raw string of json and narrows it down to be more focused, given the operations the user intends to apply to it.

Example: If I want the 10th index of an array, I do not bother parsing any other elements of the array.

I could write a generalised function for this kind of thing, and I probably will later, but for now it's not here.

The type of `Optimiser` is `([JSONOperation] -> String -> (String, [JSONOperation]))`. It takes a list of operations you intend to apply and the raw json string itself, returning a tuple containing the stripped down string result of the optimisation and the remaining actions left to complete, since some actions may have been optimally completed by your optimiser.

After writing your optimiser, you create your extractor, which you will use to extract data from json. You create this extractor using the function `optimisedExtract`, which takes an `Optimiser` and returns a function whose signiture is: `[JSONOperation] -> String -> Maybe Value`.

Once you have this function, you simply give a list of operations to apply to the raw json string and it returns you a value if everything goes well.

### Example

If you go into example/ and run `./try-example.sh` it will compile Pelias and the example and you'll be in the haskell REPL. Type the following:

```
json <- readFile "eatcheap.json"
seedyExtract [Index 0, Get "commit", Get "committer", Get "date"] json
```

and you should see `Just (SValue "2017-01-07T15:52:30Z")`, which if you look in the json file, is the value of commit->committer->date for the 0th element of the top level json array, as expected.

Use pattern matching to extract the actual string obviously.

Final quick note: Numbers come up as strings because I haven't implemented a way of converting json number format into haskell Ints yet. Should be very easy - I'll do it later.
