module Sean.Validate (PlayableStory, Problems(..), StoryError(..), validate) where

import Prelude (pure, (<<<))
import Data.Either (Either)
import Data.NonEmpty (NonEmpty)
import Sean.Types

-- the type of a validated Story
newtype PlayableStory
  = PlayableStory Story

newtype Problems
  = Problems (NonEmpty Array StoryError)

data StoryError
  = DuplicateKey Key

validate :: Story -> Either Problems PlayableStory
validate = pure <<< PlayableStory
