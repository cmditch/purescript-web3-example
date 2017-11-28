module Main where

import Prelude

import App.App (appClass)
import App.MaterialUI (muiThemeProviderClass)
import App.TestBox (getSupply)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Maybe (fromJust)
import Network.Ethereum.Web3 (ETH)
import Partial.Unsafe (unsafePartial)
import React as R
import ReactDOM (render)
import Type.Row.Effect.Equality (effFrom)

main :: forall eff. Eff (dom :: DOM | eff) Unit
-- main = launchAff getSupply
main = void (elm' >>= render ui)
  where
    ui :: R.ReactElement
    ui = R.createElement muiThemeProviderClass unit
         [R.createFactory appClass unit]
    elm' :: Eff (dom :: DOM | eff) Element
    elm' = do
      doc <- window >>= document
      elm <- getElementById (ElementId "app") (documentToNonElementParentNode (htmlDocumentToDocument doc))
      pure $ unsafePartial (fromJust elm)
