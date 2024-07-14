module Site.Utils where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Maybe (maybe)
import Effect.Aff (Aff)
import Effect.Exception (error)
import Halogen.Aff as HA
import Halogen.HTML.Core (class IsProp)
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP
import Web.DOM.ParentNode (QuerySelector(QuerySelector))
import Web.HTML.HTMLElement (HTMLElement)

data Env = Prod | Dev
derive instance eqEnv :: Eq Env

mkHP :: ∀ value a b. IsProp value => String -> value -> HP.IProp a b
mkHP name = HP.prop (HC.PropName name)

-- | Finds the element and throws if it cannot be found. Call after the document has loaded.
selectElement_ :: String -> Aff HTMLElement
selectElement_ name = do
    elem <- HA.selectElement (QuerySelector name)
    maybe (throwError <<< error $ "No elements matched the query selector `" <> name <> "`") pure elem
