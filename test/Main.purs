module Test.Main where

import Prelude
import Effect (Effect)
import Main (Expr(..), Filter(..), parseContext, check)
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
    test "Match" do
      let assertCheck f = Assert.assert ("Filter should match " <> show f) $ check task f
      assertCheck $ Plus "foo"
      assertCheck $ Plus "bar"
      assertCheck $ Project "fun.code"
      assertCheck $ Project "fun"
      assertCheck $ Minus "qux"
    test "No match" do
      let assertNoCheck f = Assert.assertFalse ("Filter should not match " <> show f) $ check task f
      assertNoCheck $ Plus "qux"
      assertNoCheck $ Project "fun.cod"
      assertNoCheck $ Minus "foo"





