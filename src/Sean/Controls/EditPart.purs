module Sean.Controls.EditPart where

import Prelude (not, ($), (<<<), (||))
import Data.Either (isRight)
import Data.Newtype (unwrap)
import Data.Traversable (traverse_)
import Sean.Data.FoldEvents as Fold
import Sean.Types
import React.Basic (Component, JSX, StateUpdate(..), createComponent, make, runUpdate)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture, capture_, targetValue)

component :: Component Props
component = createComponent "EditPart"

type Props
  = {}

data Action
  = PartKeyChange (Fold.Event String)
  | PartTextChange (Fold.Event String)

editPart ::
  Props ->
  JSX
editPart = make component { initialState, render }
  where
  initialState =
    { partKey: Fold.createEmpty unwrap toKey
    , partText: Fold.createEmpty unwrap toStoryText
    }

  update self = case _ of
    PartKeyChange event ->
      Update
        (self.state { partKey = Fold.log event (self.state.partKey) })
    PartTextChange event ->
      Update
        (self.state { partText = Fold.log event (self.state.partText) })

  send = runUpdate update

  render self =
    R.div
      { children: [ partKey, partText ]
      }
    where
    partKeyValid = isRight (Fold._getEitherOutput self.state.partKey) || (not (Fold._hasBlurred self.state.partKey))

    partKey =
      R.div
        { className: if partKeyValid then "box valid" else "box invalid"
        , children: [ R.text "Key: ", partKeyInput ]
        }

    partKeyInput =
      R.input
        { onBlur: capture_ $ send self (PartKeyChange Fold.OnBlur)
        , onFocus: capture_ $ send self (PartKeyChange Fold.OnFocus)
        , onChange: capture targetValue (\a -> traverse_ (send self <<< PartKeyChange <<< Fold.OnChange) a)
        , type: "text"
        }

    partTextValid = isRight (Fold._getEitherOutput self.state.partText) || (not (Fold._hasBlurred self.state.partText))

    partText =
      R.div
        { className: if partTextValid then "box valid" else "box invalid"
        , children: [ R.text "Text: ", partTextInput ]
        }

    partTextInput =
      R.input
        { onBlur: capture_ $ send self (PartTextChange Fold.OnBlur)
        , onFocus: capture_ $ send self (PartTextChange Fold.OnFocus)
        , onChange: capture targetValue (\a -> traverse_ (send self <<< PartTextChange <<< Fold.OnChange) a)
        , type: "text"
        }
