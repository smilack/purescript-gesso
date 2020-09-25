module Gesso.Environment (Environment(..)) where

import Effect.Ref (Ref)

type Environment appState more
  = { appState :: Ref appState | more }
