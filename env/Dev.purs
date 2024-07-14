-- | This module will only be included if spago.dhall does not
-- | detect the production environment variable.
module Site.Env where

import Site.Utils (Env(..))

env :: Env
env = Dev