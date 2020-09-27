module Gesso.Environment (Environment(..)) where

import Effect.Aff.Bus (BusRW)
import Effect.Ref (Ref)

type Environment appState more
  = { appState :: Ref appState
    , stateBus :: BusRW appState
    | more
    }
