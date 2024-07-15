module Site.Components where

import Prelude

import Effect.Aff.Class (class MonadAff, liftAff)
import Electron.IpcCall as ElectronIpc
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Site.Utils (css, cssMerge)

data Action = OpenPicker
type State = String

-- This is kind of a hack but it's necessary.
-- Why?
-- Hard coding head content into index.html is totally reasonable, unless you want to have different head contents for dev and prod builds
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
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
    where
    initialState :: ∀ input. input -> State
    initialState _ = ""

    render :: ∀ slots. State -> H.ComponentHTML Action slots m
    render filepath = HH.div
        [ css "flex flex-col justify-center items-center w-full min-h-screen bg-blue-300" ]
        [ HH.button
            [ HP.id "btn"
            , HE.onClick \_ -> OpenPicker
            , css "flex justify-center items-center w-2/3 md:w-96 h-10 my-2 p-6 border rounded-full border-gray-800 text-center uppercase text-base font-[400] text-gray-800 hover:text-gray-200 hover:bg-gray-900"
            ] [ HH.text "Open a File" ]
        , HH.text "File path: "
        , HH.strong [ HP.id "filePath" ] [ HH.text filepath ]
        ]

    handleAction = case _ of
      OpenPicker -> do
        path <- liftAff $ ElectronIpc.openFile
        H.put path
