module Sean.Controls.EditPart where

import Prelude
import React.Basic (Component, JSX, StateUpdate(..), createComponent, make, runUpdate)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

component :: Component Props
component = createComponent "EditPart"

type Props
  = {}

data Action
  = Increment

editPart ::
  Props ->
  JSX
editPart = make component { initialState, render }
  where
  initialState = { partKey: "" }

  update self = case _ of
    Increment ->
      Update
        (self.state { partKey = self.state.partKey <> " " })

  send = runUpdate update

  render self =
    R.button
      { onClick: capture_ $ send self Increment
      , children: [ R.text "what" ]
      }
