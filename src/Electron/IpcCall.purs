-- | This module imports each Electron IPC call
-- | Drawing the FFI boundary here leaves more vanilla js in our project,
-- | But without an actively maintained PureScript Electron library, this is the easiest for me to manage.

module Electron.IpcCall where

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)
import Prelude

foreign import openFileImpl :: Effect (Promise String)

openFile :: Aff String
openFile = toAffE openFileImpl
