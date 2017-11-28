module App.ContractForm where

import Prelude

import Contracts.ERC20 as ERC20
import Contracts.TestContract as TC
import Control.Error.Util (note)
import Control.Monad.Aff (Aff, Milliseconds(..), attempt, delay, launchAff, launchAff_, liftEff')
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Console as EffC
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.List.Trans (ListT(..))
import Control.Monad.Trans.Class (lift)
import DOM.HTML.HTMLElement (offsetHeight)
import DOM.Websocket.BinaryType (BinaryType(..))
import Data.Array ((!!))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Int (fromString)
import Data.List (List)
import Data.Maybe (Maybe(..), maybe)
import Data.Options ((:=))
import Data.Tuple (Tuple(..))
import MaterialUI (EventHandlerOpt(..), UnknownType(..), stringNode)
import MaterialUI.RaisedButton as RaisedButton
import MaterialUI.TextField as TextField
import Network.Ethereum.Web3 (type (:&), Address, BigNumber, D2, D5, D6, ETH, EventAction(..), IntN, UIntN, Web3, decimal, event, metamask, mkAddress, mkHexString, parseBigNumber, runWeb3, uIntNFromBigNumber, unHex)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Network.Ethereum.Web3.Solidity (Tuple3(..))
import Network.Ethereum.Web3.Solidity.Int (intNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Size (class KnownSize)
import Network.Ethereum.Web3.Types.BigNumber (embed)
import Network.Ethereum.Web3.Types.Types (CallMode(..))
import Partial.Unsafe (unsafePartial)
import React as R
import React.DOM as D
import React.DOM.Props as P
import Thermite as T
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- SimpleStorage Class
--------------------------------------------------------------------------------

type ContractFormState =
  { contractAddress :: Maybe Address
  , userAddress :: Maybe Address
  , someNum :: Maybe BigNumber
  , events :: Array String
  , errorMessage :: String
  , txId :: String
  }

initialContractFormState :: ContractFormState
initialContractFormState =
    { contractAddress: Nothing
    , userAddress: Nothing
    , someNum: Nothing
    , events: []
    , errorMessage: ""
    , txId: ""
    }

data ContractFormAction
  = UpdateCount String
  | Submit


type ContractFormProps =
  { statusCallback :: String -> T.EventHandler }


contractFormSpec :: forall eff. T.Spec (eth :: ETH | eff) ContractFormState ContractFormProps ContractFormAction
contractFormSpec = T.simpleSpec performAction render
  where
    render :: T.Render ContractFormState ContractFormProps ContractFormAction
    render dispatch props state _ =
      [ D.div [P.className ""]
         [ D.h3 [P.className "error-message-container"]
           [D.text state.errorMessage]
         , D.form [P.className "count-form"]
           [ D.div'
             [ TextField.textField (TextField.onChange := (EventHandlerOpt <<< R.handle $ \e ->
                                      dispatch (UpdateCount (unsafeCoerce e).target.value))
                                 <> TextField.hintText := stringNode "liek 42 of courze"
                                 <> TextField.fullWidth := true
                                 <> TextField.floatingLabelText := stringNode "Enter an int8 ( -128 to 127 )"
                                 ) []
             ]
            , D.div [P.className "submit-button-container"]
              [ RaisedButton.raisedButton ( RaisedButton.onClick := (EventHandlerOpt <<< R.handle $ \_ -> dispatch Submit)
                                          <> RaisedButton.backgroundColor := "#2196F3"
                                          <> RaisedButton.fullWidth := true
                                        ) [ D.div
                                            [ P.className "submit-button-text" ]
                                            [ D.text "Submit" ]
                                          ]
              ]
            , D.div' $ D.text <$> state.events
            ]
         ]
      ]

    performAction :: T.PerformAction (eth :: ETH | eff) ContractFormState ContractFormProps ContractFormAction
    performAction (UpdateCount n) _ _ = void <<< T.modifyState $ _{ someNum = parseBigNumber decimal n }
    performAction Submit props st = do
      let args = do
            userAddress <- note "Please enter a valid ethereum address" st.userAddress
            someNum <- note "Please enter a proper int8" $ intNFromBigNumber =<< st.someNum
            pure $ Tuple userAddress someNum
      case args of
        Left err -> void <<< T.modifyState $ _{ errorMessage = err }
        Right (Tuple userAddress someNum) -> void do
          txId <- lift $ runWeb3 metamask $ TC.triggerEvent st.contractAddress userAddress someNum
          lift <<< unsafeCoerceAff <<< liftEff $ props.statusCallback $ "TxId: 0x" <> (unHex txId)
          T.modifyState $ _{ errorMessage = "", txId = unHex txId }


contractFormClass :: R.ReactClass ContractFormProps
contractFormClass =
  let {spec} = T.createReactSpec contractFormSpec initialContractFormState
  in R.createClass $ spec { componentWillMount = populateAddressFields, componentDidMount = monitorContractEvents }
  where
      populateAddressFields this = void <<< launchAff $ do
        let mContractAddress = mkAddress =<< mkHexString "0xd7973a06439ddb306a3b59810562c90080bd2e55"
        mUserAddress <- getMetamaskAddress
        liftEff $ R.transformState this _{ userAddress = mUserAddress, contractAddress = mContractAddress }

      monitorContractEvents :: R.ComponentDidMount ContractFormProps ContractFormState (eth :: ETH, console :: CONSOLE)
      monitorContractEvents this = void do
        props <- R.getProps this
        state <- R.readState this
        case mkAddress =<< mkHexString "0xd7973a06439ddb306a3b59810562c90080bd2e55" of
          Nothing -> do
            launchAff $ log "Something went wrong with getting contract address"
          Just contractAddress ->
            launchAff do
              delay (Milliseconds 3000.0)
              log $ show contractAddress
              void $ runWeb3 metamask $
              event contractAddress $ \(TC.Add addressFrom someInt) -> do
                liftEff $ EffC.log $ "Some Event Occurred " <> show someInt <> show addressFrom
                liftEff <<< R.transformState this $ _{ events = [show someInt] <> state.events }
                liftEff $ props.statusCallback $ show someInt <> "  -w00t-  " <> show addressFrom
                pure ContinueEvent



getMetamaskAddress :: forall eff . Aff (eth :: ETH | eff) (Maybe Address)
getMetamaskAddress = do
  accounts <- runWeb3 metamask $ eth_getAccounts
  pure $ accounts !! 0
