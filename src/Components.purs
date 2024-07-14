module Site.Components where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Site.HTML as HTML
import Type.Proxy (Proxy(..))

type Slots =
    ( navbar :: forall query. H.Slot query Void Unit
    , faq :: forall query. H.Slot query Void Unit
    )

-- This is kind of a hack but it's necessary.
-- Why?
-- Hard coding head content into index.html is totally reasonable, unless you want to include livejs for dev and not prod builds
-- Rendering head tags in halogen fixes environment swapping, but the head element doesn't start empty like halogen expects it to.
-- The hack is to just append each tag one at a time. To see how this is used, see Main.
head ∷ ∀ a q i o m. HH.HTML (H.ComponentSlot a m Unit) Unit -> H.Component q i o m
head elem =
    H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = pure }
    }
    where
    initialState _ = elem

    render state = state

body ∷ ∀ q i o m. MonadAff m => H.Component q i o m
body =
    H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = pure }
    }
    where
    initialState :: ∀ input. input -> Unit
    initialState _ = unit

    render :: ∀ state action. state -> H.ComponentHTML action Slots m
    render _ = HTML.body
