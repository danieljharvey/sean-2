module Main (module Sean, main) where

import Sean.Types (Ending(..), Key(..), Link(..), LinkText(..), Part(..), Story(..), StoryText(..), Title(..)) as Sean
import Sean.Initial (endingLink, initialEnding, initialStart, initialStory) as Sean
import Sean.Validate (PlayableStory, Problems(..), StoryError(..), validate) as Sean
import Prelude
import Sean.App (app)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  container <- getElementById "container" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> throw "Container element not found."
    Just c ->
      let
        app' = app { label: "Increment" }
      in
        render app' c
