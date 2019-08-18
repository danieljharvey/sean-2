module Sean.Initial where

import Prelude (mempty)
import Data.Map as Map
import Data.NonEmpty (singleton)
import Sean.Types

initialStory :: Story
initialStory =
  Story
    { title: Title "A nice story"
    , start: initialStart
    , parts: mempty
    , endings: Map.singleton (Key "ending") initialEnding
    }

initialStart :: Part
initialStart =
  Part
    { partKey: Key "start"
    , partText: StoryText "It is the start of the story"
    , partLinks: singleton endingLink
    }

emptyPart :: Part
emptyPart =
  Part
    { partKey: Key ""
    , partText: StoryText ""
    , partLinks: singleton endingLink
    }

initialEnding :: Ending
initialEnding =
  Ending
    { endingText: StoryText "The end."
    }

endingLink :: Link
endingLink =
  Link
    { linkKey: Key "ending"
    , linkText: LinkText "Finish this story already"
    }
