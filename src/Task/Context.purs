module Task.Context (matchContext, enumerate, check) where

import Prelude (Ordering, compare, eq, join, map, not, (#), ($), (<>))
import Task.Types (Expr(..), Filter(..), Match(..), MinimalTask, Context)
import Data.Traversable (elem, sequence) 
import Data.Array (concatMap, filter, head, length, sortBy) 
import Data.Maybe (fromMaybe, isJust)
import Data.String (Pattern(..), stripPrefix)
import Data.Function (on) 

-- | Given a context (a complex filter) and a task, return
-- | a minimal set of changes that could be made to the 
-- | task to make it visible by the context.
-- | 
-- | We do this by:
-- |   * enumerating all the possible combinations
-- |     (see `enumerate` below for details)
-- |   * removing any that are contradicted by the current task. e.g.:
-- |       * `-foo` vs `+foo`
-- |       * `project:foo` vs `project:bar`
-- |   * finding the smallest set of changes required.
-- |     These changes could be for example:
-- |       * Add `+tag`
-- |       * set a project (where previously blank)
-- |       *... or specialise it by adding a level in the hierarchy
-- |       (e.g. `project:foo` -> `project:foo.bar`)
-- |   * If multiple options could match, we prefer the left-most
-- |     (essentially the option we come to sooner if we expand all the
-- |     nested parentheses, and's and or's systematically)
matchContext :: forall r. Context -> MinimalTask r -> Array Filter
matchContext x t =
  x # enumerate
    # map (map $ check t)
    # filter (\row -> not (Contradict `elem` row))
    # map (concatMap justNeeds)
    # sortBy compareLength
    # head
    # fromMaybe []

-- | Expand an expression into all possible values.
-- | For example, with the Context
-- |    `+foo or (+bar (+baz or +qux) -foo)`
-- | We could expand to the following options:
-- |   * `+foo`
-- |   * `+bar +baz -foo`
-- |   * `+bar +qux -foo`
enumerate :: forall a. Expr a -> Array (Array a)
enumerate (Expr x) = [[x]]
enumerate (And xs) = map join $ sequence $ map enumerate xs
enumerate (Or xs) = concatMap enumerate xs

-- | Check if a filter:
-- |   * is already `Satisfy`'d by a task
-- |   * is `Contradict`ed by a task
-- |   * could be accommodated, but `Need`'s a modification
-- |
-- | The rules are:
-- | 
-- |     | Task has    | Filter wants  |   Result          |
-- |     | ----------- | ------------- | ----------------- |
-- |     | +foo        |  +foo         |  Satisfy          |
-- |     |             |  +foo         |  Need +foo        |
-- |     | +foo        |  -foo         |  Contradict       |
-- |     |             |  -foo         |  Satisfy          |
-- |     |             |  pro:foo      |  Need pro:foo     |
-- |     | pro:foo     |  pro:foo      |  Satisfy          |
-- |     | pro:foo.bar |  pro:foo      |  Satisfy          |
-- |     | pro:foo     |  pro:foo.bar  |  Need pro:foo.bar |
-- |     | pro:foo     |  pro:bar      |  Contradict       |
-- |
-- | Note that Projects are satisfied only hierarchically,
-- | e.g. by `.` separated expressions, whereas TaskWarrior
-- | itself currently accepts left-most match of partial
-- | project names.
-- |
-- | NB: Unhandled `Other` expressions currently cause a contradiction,
-- | but this may change if it's more useful to ignore them.
check :: forall r. MinimalTask r -> Filter -> Match
check m (Plus t) | t `elem` m.tags = Satisfy
check _ p@(Plus _) = (Need p)
check m (Minus t) | t `elem` m.tags = Contradict
check _ (Minus _) = Satisfy
check m (Project p) | (m.project `eq` p) = Satisfy
check m (Project p) | (m.project `startsWith` Pattern (p <> ".")) = Satisfy
check m n@(Project p) | (p `startsWith` Pattern (m.project  <> ".")) = Need  n
check _ _ = Contradict

justNeeds :: Match -> Array Filter
justNeeds (Need f) = [f]
justNeeds _ = []

startsWith :: String -> Pattern -> Boolean
startsWith s prefix = isJust $ stripPrefix prefix s

compareLength :: forall a. Array a -> Array a -> Ordering
compareLength = compare `on` len
  -- type sig needed to disambiguate for some reason
  where len :: forall b. Array b -> Int
        len = length
