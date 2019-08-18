module Test.Main where

import Prelude (Unit, ($))
import Data.Either (isRight)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Sean.Initial (initialStory)
import Sean.Validate

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Sean's Big Day" do
          describe "Validations" do
            it "Detects a non-broken Story" do
              let
                story = validate initialStory
              (isRight story) `shouldEqual` true
