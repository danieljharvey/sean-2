module Sean.Types where

import Prelude
import Data.Either (Either(..))
import Data.Lens (Lens')
import Data.Lens.Index (ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Traversal (Traversal', traversed)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty)
import Data.Symbol (SProxy(..))

newtype Title
  = Title String

newtype Key
  = Key String

derive instance newtypeKey :: Newtype Key _

toKey :: String -> Either PartError Key
toKey s = case s of
  "" -> Left EmptyKey
  a -> Right (Key a)

derive newtype instance eqKey :: Eq Key

derive newtype instance ordKey :: Ord Key

newtype StoryText
  = StoryText String

derive instance newtypeStoryText :: Newtype StoryText _

toStoryText :: String -> Either PartError StoryText
toStoryText s = case s of
  "" -> Left EmptyKey
  a -> Right (StoryText a)

newtype LinkText
  = LinkText String

data PartError
  = EmptyKey

---
newtype Link
  = Link
  { linkKey :: Key
  , linkText :: LinkText
  }

derive instance newtypeLink :: Newtype Link _

_LinkKey :: Lens' Link Key
_LinkKey =
  _Newtype
    <<< prop (SProxy :: SProxy "linkKey")

_LinkText :: Lens' Link LinkText
_LinkText =
  _Newtype
    <<< prop (SProxy :: SProxy "linkText")

---
newtype Part
  = Part
  { partKey :: Key
  , partText :: StoryText
  , partLinks :: NonEmpty Array Link
  }

derive instance newtypePart :: Newtype Part _

_PartKey :: Lens' Part Key
_PartKey =
  _Newtype
    <<< prop (SProxy :: SProxy "partKey")

_PartText :: Lens' Part StoryText
_PartText =
  _Newtype
    <<< prop (SProxy :: SProxy "partText")

_PartLinks :: Traversal' Part Link
_PartLinks =
  _Newtype
    <<< prop (SProxy :: SProxy "partLinks")
    <<< traversed

---
newtype Ending
  = Ending
  { endingText :: StoryText
  }

derive instance newtypeEnding :: Newtype Ending _

_EndingText :: Lens' Ending StoryText
_EndingText =
  _Newtype
    <<< prop (SProxy :: SProxy "endingText")

---
newtype Story
  = Story
  { title :: Title
  , start :: Part
  , parts :: Map Key Part
  , endings :: Map Key Ending
  }

derive instance newtypeStory :: Newtype Story _

_Title ::
  Lens' Story Title
_Title =
  _Newtype
    <<< prop (SProxy :: SProxy "title")

_Start ::
  Lens' Story Part
_Start =
  _Newtype
    <<< prop (SProxy :: SProxy "start")

_Parts ::
  Key ->
  Traversal' Story Part
_Parts k =
  _Newtype
    <<< prop (SProxy :: SProxy "parts")
    <<< ix k

_Endings ::
  Key ->
  Traversal' Story Ending
_Endings k =
  _Newtype
    <<< prop (SProxy :: SProxy "endings")
    <<< ix k
