module Sean.Controls.EditLink where

import Prelude
import Data.Either (isRight)
import Data.Newtype (unwrap)
import Data.Traversable (traverse_)
import Effect
import Sean.Data.FoldEvents as Fold
import Sean.Types
import React.Basic (Component, JSX, StateUpdate(..), createComponent, make, runUpdate)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture, capture_, targetValue)

component :: Component Props
component = createComponent "EditLink"

type Props
  = { submit :: Link -> Effect Unit
    }

data Action
  = LinkKeyChange (Fold.Event String)
  | LinkTextChange (Fold.Event String)

editLink ::
  Props ->
  JSX
editLink = make component { initialState, render }
  where
  initialState =
    { linkKey: Fold.createEmpty unwrap toKey
    , linkText: Fold.createEmpty unwrap toLinkText
    }

  update self = case _ of
    LinkKeyChange event ->
      Update
        (self.state { linkKey = Fold.log event (self.state.linkKey) })
    LinkTextChange event ->
      Update
        (self.state { linkText = Fold.log event (self.state.linkText) })

  send = runUpdate update

  render self =
    R.div
      { children: ([ linkKey, linkText ] <> submitButton)
      }
    where
    submitButton =
      if linkKeyValid && linkTextValid then
        [ R.button
            { children: [ R.text "Submit" ] }
        ]
      else
        []

    linkKeyValid = isRight (Fold._getEitherOutput self.state.linkKey)

    showLinkKeyError = Fold._hasBlurred self.state.linkKey

    linkKey =
      R.div
        { className:
          if linkKeyValid || not showLinkKeyError then
            "box valid"
          else
            "box invalid"
        , children: [ R.text "Key: ", linkKeyInput ]
        }

    linkKeyInput =
      R.input
        { onBlur: capture_ $ send self (LinkKeyChange Fold.OnBlur)
        , onFocus: capture_ $ send self (LinkKeyChange Fold.OnFocus)
        , onChange: capture targetValue (\a -> traverse_ (send self <<< LinkKeyChange <<< Fold.OnChange) a)
        , type: "text"
        }

    linkTextValid = isRight (Fold._getEitherOutput self.state.linkText) 

    showLinkTextError = Fold._hasBlurred self.state.linkText

    linkText =
      R.div
        { className:
          if linkTextValid || not showLinkTextError then
            "box valid"
          else
            "box invalid"
        , children: [ R.text "Text: ", linkTextInput ]
        }

    linkTextInput =
      R.input
        { onBlur: capture_ $ send self (LinkTextChange Fold.OnBlur)
        , onFocus: capture_ $ send self (LinkTextChange Fold.OnFocus)
        , onChange: capture targetValue (\a -> traverse_ (send self <<< LinkTextChange <<< Fold.OnChange) a)
        , type: "text"
        }
