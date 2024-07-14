module Main where

import Prelude

import Data.Foldable (traverse_)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Site.HTML as HTML
import Site.Components as Components
import Site.Env (env)
import Site.Utils (selectElement_)

-- this is the electron renderer
main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  -- head starts non-empty so append all the other head elements to it one at a time
  -- append behavior is undocumented so other VDOM driver implementations may be different
  traverse_ (\elem -> runUI (Components.head elem) unit =<< selectElement_ "head") (HTML.headContents env)
  body <- selectElement_ "body"
  runUI Components.body unit body

-- this is the electron preload
preload :: Effect Unit
preload = pure unit
