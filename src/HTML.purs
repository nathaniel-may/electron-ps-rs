module Site.HTML
    ( headContents
    , body ) where

import Prelude

import Data.Maybe (maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Site.Utils (Env(Dev, Prod), mkHP)

headContents :: ∀ a b. Env -> Array (HH.HTML a b)
-- add any debugging or auto refresh bits for dev here
headContents Dev = head_base
headContents Prod = head_base

head_base :: ∀ a b. Array (HH.HTML a b)
head_base =
    [ HH.title_ [ HH.text "File Picker" ]
    , HH.meta [ mkHP "http-equiv" "Content-Security-Policy", mkHP "content" "default-src 'self'; script-src 'self'" ]
    ] <> meta

meta :: ∀ a b. Array (HH.HTML a b)
meta = map metaFrom
    -- some have HP values, but some dont so I'm reverting
    -- to use strings for all of them.
    [ Tuple "charset" "utf-8"
    , Tuple "viewport" "width=device-width, initial-scale=1.0"
    ]

metaFrom :: ∀ a b. Tuple String String -> HH.HTML a b
metaFrom kv = HH.meta [ HP.name (fst kv), mkHP "content" (snd kv) ]

body :: ∀ a b. HH.HTML a b
body =
    HH.div_
        [ HH.button [ HP.id "btn" ] [ HH.text "Open a File" ]
        , HH.text "File path: "
        , HH.strong [ HP.id "filePath" ] []
        ]
        -- <button type="button" id="btn">Open a File</button>
        -- File path: <strong id="filePath"></strong>
        -- <script src="./renderer.js"></script>
    where
        -- TODO add `before:backdrop-blur-[2px]` so it takes effect somehow
        heroBefore = "before:h-min-screen before:h-max-screen before:absolute before:bg-center before:bg-no-repeat before:bg-cover before:bg-fixed before:inset-0 before:bg-hero before:opacity-70"
