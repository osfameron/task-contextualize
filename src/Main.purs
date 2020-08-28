module Main where

import Prelude 
import Effect (Effect)
import Effect.Console (log)
import Data.Traversable (sequence)
import Data.Array (concatMap, intercalate)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Text.Parsing.StringParser
import Text.Parsing.StringParser.Combinators
import Text.Parsing.StringParser.CodePoints
import Data.Maybe
import Control.Plus

data Expr a = Expr a
            | And (Array (Expr a))
            | Or (Array (Expr a))

instance showExpr :: Show a => Show (Expr a) where
  show (Expr e) = "Expr " <> (show e)
  show (And es) = "And [" <> intercalate ", " (map show es) <> "]"
  show (Or es) = "Or [" <> intercalate ", " (map show es) <> "]"

simplify :: forall a. Expr a -> Expr a
simplify (And es) = And $ concatMap simplify' es
simplify (Or es) = Or $ concatMap simplify' es
simplify e = e

simplify' :: forall a. Expr a -> Array (Expr a)
simplify' (And []) = []
simplify' (Or []) = []
simplify' (And [e]) = simplify' e
simplify' (Or [e]) = simplify' e
simplify' (And es) = [And $ concatMap simplify' es]
simplify' (Or es) = [Or $ concatMap simplify' es]
simplify' e = [e]

data Filter = Plus String
            | Minus String
            | Project String

filter :: Parser (Maybe Filter)
filter = optionMaybe $ try tag <|> try project

skip :: forall a. Parser a -> Parser Unit
skip = void

tag :: Parser Filter
tag = plus <|> minus

project :: Parser Filter
project = do
  skip $ regex "pro(j(e(ct?)?)?)?:"
  name <- regex "\\w+"
  pure $ Project name

plus :: Parser Filter
plus = do
  skip $ string "+"
  name <- regex "[a-z]\\w+"
  pure $ Plus name

minus :: Parser Filter
minus = do
  skip $ string "-"
  name <- regex "[a-z]\\w+"
  pure $ Minus name 

derive instance genericFilter :: Generic Filter _
instance showFilter :: Show Filter where
  show = genericShow


type Task = { id :: Number
            , description :: String
            , entry :: String -- date
            , modified :: String -- date
            , project :: String
            , status :: String -- enum
            , tags :: Array String
            , uuid :: String
            , annotations :: Array { entry :: String, description :: String }
            , urgency :: Number }

taskFromString :: String -> Either JsonDecodeError Task
taskFromString s = decodeJson =<< parseJson s


j :: String
j = "{\"id\":6,\"description\":\"Q: next distributed systems course\",\"entry\":\"20200824T080908Z\",\"modified\":\"20200825T091250Z\",\"project\":\"bbc\",\"status\":\"pending\",\"tags\":[\"arch\",\"louis.moselhi\",\"sched\"],\"uuid\":\"2b9dcbc8-3209-4501-bf8c-2d7e606ea68c\",\"annotations\":[{\"entry\":\"20200824T080916Z\",\"description\":\"https:\\/\\/www.bbc.co.uk\\/academy\\/en\\/courses\\/COU-3041\"},{\"entry\":\"20200825T090921Z\",\"description\":\"remove this, as poor reviews, ping Louis\"},{\"entry\":\"20200825T091250Z\",\"description\":\"and\\/or speak to KPL, review what value it could give\"}],\"urgency\":4.81096}"

main :: Effect Unit
main = do
  log "🍝"

example :: Expr Filter
example = And [Expr $ Plus "foo", Or [Expr $ Minus "bar", Expr $ Project "qux"]]

enumerate :: forall a. Expr a -> Array (Array a)
enumerate (Expr x) = [[x]]
enumerate (And xs) = map join $ sequence $ map enumerate xs
enumerate (Or xs) = concatMap enumerate xs
