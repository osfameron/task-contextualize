-- | Using [argonaut-codecs](https://pursuit.purescript.org/packages/purescript-argonaut-codecs/) it's easy to
-- | decode JSON given a record type, modify the record, and then encode back to JSON.
-- | Note that the type you define doesn't have to match all the fields in the JSON, *but* in that case
-- | those fields are lost. When you come to write the data back, you've lost that extra data.
-- | 
-- | This "passthrough" library lets you decorate a record with a `_json` field that contains the original
-- | JSON. On re-encoding, any fields that don't exist in your record will default to their original
-- | value. This may be useful for user-defined fields that your code doesn't know about, but which you
-- | want to preserve.
-- |
-- | ## An example
-- |
-- |      -- decorate your record `WithJsonPassthrough`
-- |      type Foo = { foo :: String, bar :: Number | WithJsonPassthrough }
-- |
-- |      decodeFoo :: Json -> Either JsonDecodeError Foo
-- |      decodeFoo s = addPassthrough s >>= decodeJson
-- |
-- |      encodeFoo :: Foo -> Either JsonDecodeError Json
-- |      encodeFoo = mergePassthrough <<< encodeJson
-- |
-- |      -- Update a field. Note that "baz" which isn't handled is passed through.
-- |      process :: Either JsonDecodeError String
-- |      process =
-- |          do
-- |              decoded <- decodeFoo =<< parseJson jsonString
-- |              let updated = decoded { foo = "Updated" }
-- |              encoded <- encodeFoo updated
-- |              pure $ stringify encoded
-- |
module Data.Argonaut.Passthrough 
    (WithJsonPassthrough, addPassthrough, mergePassthrough) 
where

import Prelude (bind, pure, ($))
import Data.Argonaut.Core (Json, fromObject, toObject)
import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Either (Either, note)
import Foreign.Object as O
import Data.Tuple (Tuple(..))

-- | Extensible row type for JSON passthrough. Use this to define your record
-- | for conversion with `decodeJson`
-- | e.g.
-- |
-- |         type Person = { first :: String, last :: String | WithJsonPassthrough }
type WithJsonPassthrough = ( _json :: Json )

-- | Add the JSON data to the passthrough field.  You may want to define your decoder
-- | like:
-- |
-- |         decodePerson :: Json -> Either JsonDecodeError Person
-- |         decodePerson s = addPassthrough s >>= decodeJson     
addPassthrough :: Json -> Either JsonDecodeError Json
addPassthrough json = do
    obj <- note (TypeMismatch "Json is not an object") $ toObject json
    let obj' = O.insert "_json" json obj
    pure $ fromObject obj'

-- | Merge the JSON data stored in the passthrough field back into the Json structure.
-- | You may want to define your encoder like:
-- |
-- |         encodePerson :: Person -> Either JsonDecodeError Json
-- |         encodePerson = mergePassthrough <<< encodeJson
mergePassthrough :: Json -> Either JsonDecodeError Json
mergePassthrough json = do
    obj <- note (TypeMismatch "Json is not an object") $ toObject json
    (Tuple json' obj') <- note MissingValue $ O.pop "_json" obj
    orig <- note (TypeMismatch "_json is not an object") $ toObject json'
    let merged = O.union obj' orig
    pure $ fromObject merged
