module Test.Main where

import Prelude (Unit, discard, pure, show, ($), (<$>), (<*>), (<>), (==))
import Effect (Effect)
import Task.Types (Expr(..), Filter(..), Match(..))
import Task.Main
import Task.Parse (parseContext)
import Task.Context (check, matchContext)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Data.Either (Either(..))

main :: Effect Unit
main = runTest do

  suite "Parsing (happy path)" do
    let parse context expected = Assert.equal (Right expected) (parseContext context)
    test "Atoms" do
      parse "pro:foo" $ Expr (Project "foo")
      parse "proj:foo" $ Expr (Project "foo")
      parse "project:foo" $ Expr (Project "foo")
      parse "+foo" $ Expr (Plus "foo")
      parse "-foo" $ Expr (Minus "foo")
    test "Combine" do
      parse "pro:foo and +foo" $
        And
          [ Expr $ Project "foo"
          , Expr $ Plus "foo"]
      parse "pro:foo +foo" $
        And
          [ Expr $ Project "foo"
          , Expr $ Plus "foo"]
      parse "pro:foo or +foo" $
        Or
          [ Expr $ Project "foo"
          , Expr $ Plus "foo"]
      parse "pro:foo and +foo and +bar" $
        And
          [ Expr $ Project "foo"
          , Expr $ Plus "foo"
          , Expr $ Plus "bar"]
      parse "pro:foo or +foo or +bar" $
        Or
          [ Expr $ Project "foo"
          , Expr $ Plus "foo"
          , Expr $ Plus "bar"]
      parse "pro:foo and +foo or +bar" $
        And
          [ Expr (Project "foo")
          , Or
            [ Expr (Plus "foo")
            , Expr (Plus "bar")]]
    test "Parens" do
      parse "(+foo)" $ Expr (Plus "foo")
      parse "(foo)" $ Expr (Other "foo")
      parse "(pro:foo and +foo) or (pro:bar +bar)" $
        Or 
          [ And [Expr (Project "foo"), Expr (Plus "foo")]
          , And [Expr (Project "bar"), Expr (Plus "bar")]]

  suite "Check" do
    let task = { id: 1, project: "fun.code", tags: ["foo", "bar"] }
    test "Satisfies" do
      let assertSatisfy f = Assert.assert ("Filter should satisfy " <> show f) $ check task f == Satisfy
      assertSatisfy $ Plus "foo"
      assertSatisfy $ Plus "bar"
      assertSatisfy $ Project "fun.code"
      assertSatisfy $ Project "fun"
      assertSatisfy $ Minus "qux"
    test "Contradicts" do
      let assertContradict f = Assert.assert ("Filter should contradict " <> show f) $ check task f == Contradict
      assertContradict $ Project "work"
      assertContradict $ Project "fun.cod"
      assertContradict $ Project "fun.codes"
      assertContradict $ Minus "foo"
    test "Need" do
      let assertNeed f = Assert.equal (Need f) (check task f)
      assertNeed $ Plus "qux"
      assertNeed $ Project "fun.code.purescript"

  suite "matchContext" do
    let task = { id: 1, project: "fun.code", tags: ["foo", "bar"] }
    let
      assertMatch c exp =
        Assert.equal (Right exp) $
          matchContext <$> (parseContext c) <*> pure task
    test "Basic" do
      assertMatch "pro:fun.code +new" [ Plus "new" ]
      assertMatch "pro:fun +new" [ Plus "new" ]
      assertMatch "pro:fun.code.purescript +new" [ Project "fun.code.purescript", Plus "new" ]
    test "Alternatives" do
      assertMatch "(+new +baz) or +qux" [ Plus "qux" ]
      assertMatch "(+new +baz) or (+qux +qux)" [ Plus "new", Plus "baz" ]
      assertMatch "+new (+foo or +baz)" [ Plus "new" ]
      assertMatch "+new (+baz or +foo)" [ Plus "new" ]
      assertMatch "+new (+baz or +qux)" [ Plus "new", Plus "baz" ]

