module Gesso.Env where

import Effect.Ref (Ref)

type Env appState
  = { appState :: Ref appState
    }
