module App.App where

import Prelude
import App.ContractForm (contractFormClass)
import Network.Ethereum.Web3 (ETH)
import React as R
import React.DOM as D
import React.DOM.Props as P
import Thermite as T

type AppState = {status :: String}

data AppAction = UpdateStatus String

appSpec :: forall eff props . T.Spec (eth :: ETH | eff) AppState props AppAction
appSpec = T.simpleSpec performAction render
  where
    render dispatch _ state _ =
      let props = {statusCallback: dispatch <<< UpdateStatus}
      in [ D.div [P.className "status-bar"] [D.text state.status]
         , R.createFactory contractFormClass props
         ]

    performAction (UpdateStatus status) _ st = void <<< T.modifyState $ _{status=status}

appClass :: forall props. R.ReactClass props
appClass = T.createClass appSpec {status: "Please enter an int8."}
