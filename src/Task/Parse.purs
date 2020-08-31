module Task.Parse
    (parseContext, simplify)
where

import Prelude (Unit, bind, discard, pure, void, ($), (/=), (<>))
import Task.Types (Context, Expr(..), Filter(..)) 
import Data.Array (concatMap, cons, snoc) 
import Data.Either (Either)
import Text.Parsing.StringParser (ParseError, Parser, runParser, try) 
import Text.Parsing.StringParser.Combinators (between, chainl1, optional) 
import Text.Parsing.StringParser.CodePoints (regex, skipSpaces, string) 
import Control.Plus ((<|>)) 
import Control.MonadZero (guard) 
import Control.Lazy (defer) 

-- | Parse a TaskWarrior [context](https://taskwarrior.org/docs/context.html) string
-- | as an expression of [filters](https://taskwarrior.org/docs/filter.html)
-- |
-- | The current filters are currently supported:
-- |
-- | * `+tag` A tag, beginning with a lower-case character (e.g. exclusing "special" tags)
-- | * `-tag` Absence of a tag
-- | * `project:foo` and abbreviations down to `pro:foo`.
-- | (NB: we don't check for UDAs with shorter names)
-- | * Every other filter is parsed as an "Other" value, and currently ignored
-- | * `and` and `or` are handled, as is implicit and in e.g. `+foo +bar`
-- |   * (NB: behaviour not fully tested against original C++ code, so precedence may not be correct)
-- | * but Parentheses are supported 
parseContext :: String -> Either ParseError Context
parseContext = runParser expr

-- | Parse an expression , which may be a single Filter, or an
-- | expression with `and` or `or`.
-- NB: recursive definition, so using `defer`
expr :: Parser Context
expr    = defer (\_ -> try node) `chainl1` orop `chainl1` andop

-- | Parse a node: e.g. a single Filter, or a parenthesized expression.
node :: Parser Context
node  = (parens $ defer (\_ -> expr)) <|> filter

-- | Parentheses
parens :: forall a. Parser a -> Parser a
parens = between (string "(")  (string ")")

-- | Supported filters are `+tag`, `-tag`, `project:foo`
-- | All other filters are returned as `Other`
filter :: Parser Context
filter = tag <|> project <|> other

-- | Parse a `+tag` or `-tag` expression
-- | NB: we specifically require the tags to start
-- | with a lower-case letter, to exclude special tags
-- | like `+LATEST` for now.
tag :: Parser Context
tag = tag' "+" Plus
  <|> tag' "-" Minus
  where
    tag' s c = do
      skip $ string s
      name <- regex "[a-z]\\w+"
      pure $ Expr $ c name

-- | Parse a project definition, `project:foo` or any abbreviation up to
-- | minimum of 3 letters `pro:foo`. Note that we do NOT currently check for
-- | any UDAs which might make this ambiguous.
project :: Parser Context
project = do
  skip $ regex "pro(j(e(ct?)?)?)?:"
  name <- regex "[\\w.]+"
  pure $ Expr $ Project name

-- | All other values are returned as `Other` for e.g. diagnostics
-- NB: we have to make sure we don't greedily parse parens, and's, or's etc.
other :: Parser Context
other = do
  token <- regex "[^\\s()]+"
  guard $ token /= "and"
  guard $ token /= "or"
  pure $ Expr $ Other token

-- | binary op to simplify two adjacent `Or` expressions
-- | e.g. `makeOr (Or [Expr 1]) (Or [Expr 2]) -> Or [Expr 1, Expr 2])` 
makeOr :: forall a. Expr a -> Expr a -> Expr a
makeOr (Or a) (Or b) = Or (a <> b)
makeOr (Or a) b = Or $ snoc a b
makeOr a (Or b) = Or $ cons a b
makeOr a b = Or [a, b]

-- | Parse an `or` expression
orop :: forall a. Parser (Expr a -> Expr a -> Expr a)
orop = try do
  skipSpaces
  skip $ string "or"
  skipSpaces
  pure makeOr

-- | binary op to simplify two adjacent `And` expressions
-- | e.g. `makeAnd (And [Expr 1]) (And [Expr 2]) -> And [Expr 1, Expr 2])` 
makeAnd :: forall a. Expr a -> Expr a -> Expr a
makeAnd (And a) (And b) = And (a <> b)
makeAnd (And a) b = And $ snoc a b
makeAnd a (And b) = And $ cons a b
makeAnd a b = And [a, b]

-- | Parse an `and` expression
andop :: forall a. Parser (Expr a -> Expr a -> Expr a)
andop = do
  skipSpaces
  optional $ string "and"
  optional skipSpaces
  pure makeAnd

-- | Skip the provided parser, discarding its output
skip :: forall a. Parser a -> Parser Unit
skip = void

-- | Simplify an expression recursively, getting rid of redundant
-- | singleton `And`s and `Or`s.
-- NB: this isn't currently used.
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
