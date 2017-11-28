module App.TestBox where

import Prelude

import Contracts.ERC20 (totalSupply)
import Contracts.TestContract (triggerEvent, Add(..))
import Control.Error.Util (note)
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Except (runExcept)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Prisms (_Object, _String)
import Data.Either (either)
import Data.EitherR (fmapL)
import Data.Foreign (renderForeignError)
import Data.Foreign.Class (decode, encode)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Network.Ethereum.Web3.Api (net_version)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider, Provider, httpProvider, metamask, runWeb3)
import Network.Ethereum.Web3.Types (Address, CallMode(..), ETH, Web3(..), mkAddress, mkHexString, unAddress, unHex)
import Type.Proxy (Proxy(..))
import App.Web3Provider (httpP)



getSupply =
  let address = mkAddress =<< mkHexString "0xd26114cd6EE289AccF82350c8d8487fedB8A0C07"
  in
    case address of
      Nothing ->
        log "blerp"
      Just address ->
        do
          supply <- runWeb3 httpP $ totalSupply address Nothing Latest
          logShow supply



get =
  let address = mkAddress =<< mkHexString "0xd26114cd6EE289AccF82350c8d8487fedB8A0C07"
  in
    case address of
      Nothing ->
        log "blerp"
      Just address ->
        do
          supply <- runWeb3 httpP $ totalSupply address Nothing Latest
          logShow supply
