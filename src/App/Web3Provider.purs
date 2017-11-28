module App.Web3Provider
  ( httpP
  , HttpProvider
  ) where

import Prelude

import Control.Monad.Aff (liftEff')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Network.Ethereum.Web3 (ETH)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider, Provider, httpProvider)
import Network.Ethereum.Web3.Types (Web3(..))
import Type.Proxy (Proxy(..))


data HttpProvider

httpP :: Proxy HttpProvider
httpP = Proxy

makeProvider :: forall eff . Eff (eth :: ETH, exception :: EXCEPTION | eff) Provider
makeProvider = unsafeCoerceEff $ do
  httpProvider "https://mainnet.infura.io/metamask:8545"

instance providerHttp :: IsAsyncProvider HttpProvider where
  getAsyncProvider = Web3 <<< liftEff' $ makeProvider
