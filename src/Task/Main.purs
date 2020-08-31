module Task.Main where

import Prelude (Unit, (=<<))
import Task.Types (Task)
import Task.Parse
import Task.Context
import Effect (Effect) 
import Effect.Console (log) 
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson) 
import Data.Either (Either) 

taskFromString :: String -> Either JsonDecodeError Task
taskFromString s = decodeJson =<< parseJson s

main :: Effect Unit
main = do
  log "🍝"
