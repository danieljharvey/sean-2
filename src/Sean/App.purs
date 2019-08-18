module Sean.App where

import React.Basic (Component, JSX, StateUpdate(..), createComponent, make, runUpdate)
import Sean.Controls.EditPart (editPart)

component :: Component Props
component = createComponent "App"

type Props
  = { label :: String
    }

data Action
  = Increment

app :: Props -> JSX
app = make component { initialState, render }
  where
  initialState = {}

  update self = case _ of
    Increment -> NoUpdate

  send = runUpdate update

  render self = editPart {}
