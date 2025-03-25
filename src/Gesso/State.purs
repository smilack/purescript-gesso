-- | Types used when dealing with multiple versions of the same thing - most
-- | often, the application state.
module Gesso.State
  ( Compare
  , History
  , States
  , lerp
  ) where

import Prelude

-- | Two different versions of the same thing. Used when the old and new
-- | versions are probably not immediately sequential - there may have been many
-- | versions between `old` and `new`.
type Compare a = { old :: a, new :: a }

-- | A record for keeping track of changes to a value which may change many
-- | times, when it is important to track the original, current, and previous
-- | values, and comparing the values directly is impossible. Useful as an
-- | accumulator when folding.
type History a =
  { old :: a
  , new :: a
  , original :: a
  , changed :: Boolean
  }

-- | Two different, sequential versions of the same thing, and the amount of
-- | progress from the earlier to the later. `t` should be in the interval
-- | `[0, 1]`.
type States a = { current :: a, previous :: a, t :: Number }

-- | Calculate the linear interpolation between two values
lerp :: States Number -> Number
lerp { current, previous, t } = (1.0 - t) * previous + t * current
