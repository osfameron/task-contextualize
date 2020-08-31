module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Traversable (sequence)
import Data.Array (concatMap, cons, intercalate, snoc, findIndex, filter, sortBy, head)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)
import Text.Parsing.StringParser (Parser, runParser, try, ParseError)
import Text.Parsing.StringParser.Combinators (between, chainl1, optional)
import Text.Parsing.StringParser.CodePoints (regex, skipSpaces, string)
import Control.Plus ((<|>))
import Control.MonadZero (guard)
import Control.Lazy (defer)
import Data.Maybe
import Data.String
import Data.Foldable (length, elem)
import Data.Function (on)
import Debug.Trace

data Expr a = Expr a
            | And (Array (Expr a))
            | Or (Array (Expr a))

instance eqExpr :: Eq a => Eq (Expr a) where
  eq (Expr a) (Expr b) = a `eq` b 
  eq (And as) (And bs) = as `eq` bs
  eq (Or as) (Or bs) = as `eq` bs
  eq _ _ = false

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
            | Other String

parseContext :: String -> Either ParseError (Expr Filter)
parseContext = runParser expr

filter' :: Parser (Expr Filter)
filter' = tag <|> project <|> other

other :: Parser (Expr Filter)
other = do
  token <- regex "[^\\s()]+"
  guard $ token /= "and"
  guard $ token /= "or"
  pure $ Expr $ Other token

makeOr :: forall a. Expr a -> Expr a -> Expr a
makeOr (Or a) (Or b) = Or (a <> b)
makeOr (Or a) b = Or $ snoc a b
makeOr a (Or b) = Or $ cons a b
makeOr a b = Or [a, b]

orop :: forall a. Parser (Expr a -> Expr a -> Expr a)
orop = try do
  skipSpaces
  skip $ string "or"
  skipSpaces
  pure makeOr

makeAnd :: forall a. Expr a -> Expr a -> Expr a
makeAnd (And a) (And b) = And (a <> b)
makeAnd (And a) b = And $ snoc a b
makeAnd a (And b) = And $ cons a b
makeAnd a b = And [a, b]

andop :: forall a. Parser (Expr a -> Expr a -> Expr a)
andop = do
  skipSpaces
  optional $ string "and"
  optional skipSpaces
  pure makeAnd

expr :: Parser (Expr Filter)
expr    = defer (\_ -> try node) `chainl1` orop `chainl1` andop

node :: Parser (Expr Filter)
node  = (parens $ defer (\_ -> expr)) <|> filter'

parens :: forall a. Parser a -> Parser a
parens = between (string "(")  (string ")")

skip :: forall a. Parser a -> Parser Unit
skip = void

tag :: Parser (Expr Filter)
tag = tag' "+" Plus
  <|> tag' "-" Minus
  where
    tag' s c = do
      skip $ string s
      name <- regex "[a-z]\\w+"
      pure $ Expr $ c name

project :: Parser (Expr Filter)
project = do
  skip $ regex "pro(j(e(ct?)?)?)?:"
  name <- regex "[\\w.]+"
  pure $ Expr $ Project name

derive instance genericFilter :: Generic Filter _
instance showFilter :: Show Filter where
  show = genericShow
instance eqFilter :: Eq Filter where
  eq = genericEq

type MinimalTask r = 
                   { id :: Int
                   , project :: String
                   , tags :: Array String
                   | r }

type Task = { id :: Int
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

compareLength :: forall a. Array a -> Array a -> Ordering
compareLength = compare `on` len
  where len :: forall b. Array b -> Int
        len = length

matchContext :: forall r. Expr Filter -> MinimalTask r -> Array Filter
matchContext x t =
  x # enumerate
    # map (map $ check t)
    # filter (\row -> not (Contradict `elem` row))
    # map (concatMap justNeeds)
    # sortBy compareLength
    # head
    # fromMaybe []

startsWith :: String -> Pattern -> Boolean
startsWith s prefix = isJust $ stripPrefix prefix s

data Match = Satisfy 
           | Contradict
           | Need Filter

derive instance genericMatch :: Generic Match _
instance showMatch :: Show Match where
  show = genericShow
instance eqMatch :: Eq Match where
  eq = genericEq

justNeeds :: Match -> Array Filter
justNeeds (Need f) = [f]
justNeeds _ = []

satisfies :: Match -> Boolean
satisfies Satisfy = true
satisfies _ = false

contradicts :: Match -> Boolean
contradicts Contradict = true
contradicts _ = false

check :: forall r. MinimalTask r -> Filter -> Match
check m (Plus t) | t `elem` m.tags = Satisfy
check m p@(Plus _) = Need p
check m (Minus t) | not $ t `elem` m.tags = Satisfy
check m (Project p) | (m.project `eq` p) = Satisfy
check m (Project p) | (m.project `startsWith` Pattern (p <> ".")) = Satisfy
check m n@(Project p) | (p `startsWith` Pattern (m.project  <> ".")) = Need  n
check _ _ = Contradict
