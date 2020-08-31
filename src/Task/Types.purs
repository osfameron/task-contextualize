module Task.Types where

import Prelude (class Eq, class Show, eq, map, show, (<>))
import Data.Traversable (intercalate) 
import Data.Generic.Rep (class Generic) 
import Data.Generic.Rep.Show (genericShow) 
import Data.Generic.Rep.Eq (genericEq) 

-- | A "Context" in TaskWarrior is an expression of various
-- | Filter 
type Context = Expr Filter

-- | `Expr` represents an expression tree of some type
-- | with `And` and `Or` branches
data Expr a = Expr a
            | And (Array (Expr a))
            | Or (Array (Expr a))

-- Expr is recursive, so doesn't play nicely with generic derivation
instance eqExpr :: Eq a => Eq (Expr a) where
  eq (Expr a) (Expr b) = a `eq` b 
  eq (And as) (And bs) = as `eq` bs
  eq (Or as) (Or bs) = as `eq` bs
  eq _ _ = false

instance showExpr :: Show a => Show (Expr a) where
  show (Expr e) = "Expr " <> (show e)
  show (And es) = "And [" <> intercalate ", " (map show es) <> "]"
  show (Or es) = "Or [" <> intercalate ", " (map show es) <> "]"

-- | `Filter` represents TaskWarrior filters.
-- | We currently handle +tag, -tag, and project:foo
-- | with a final `Other` constructor to represent
-- | unhandled types.
data Filter = Plus String
            | Minus String
            | Project String
            | Other String

derive instance genericFilter :: Generic Filter _
instance showFilter :: Show Filter where
  show = genericShow
instance eqFilter :: Eq Filter where
  eq = genericEq

-- | A minimal TaskWarrior task, for testing
type MinimalTask r = 
                   { id :: Int
                   , project :: String
                   , tags :: Array String
                   | r }

-- | Full TaskWarrior task structure 
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

-- | A `Match` represents how a `Filter` applies to a given task.
-- | * does it `Satisfy` the requirement?
-- | * or `Contradict` it? (e.g. a tag, where `-tag` was required)
-- | * or could it satisfy if a `Need` was filled (like setting a tag or project.
data Match = Satisfy 
           | Contradict
           | Need Filter

derive instance genericMatch :: Generic Match _
instance showMatch :: Show Match where
  show = genericShow
instance eqMatch :: Eq Match where
  eq = genericEq
