module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Main
import Test.Unit
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Data.Either

main :: Effect Unit
main = runTest do
  suite "Parsing (happy path)" do
    let parse context expected = Assert.equal (parseContext context) (Right expected)
    test "Atoms" do
      parse "pro:foo" (Expr (Project "foo"))
      parse "proj:foo" (Expr (Project "foo"))
      parse "project:foo" (Expr (Project "foo"))
      parse "+foo" (Expr (Plus "foo"))
      parse "-foo" (Expr (Minus "foo"))

