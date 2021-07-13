module State
  ( State
  , newState
  , craftState
  , mutate
  , mutateLocation
  , mutateFilesystem
  , setLocation
  , setFilesystem
  ) where

import Prelude

import Filesystem (Filesystem, Location, emptyLocation)

type State
  = { filesystem :: Filesystem
    , location :: Location
    }

newState :: Filesystem -> State
newState fs = craftState fs emptyLocation

craftState :: Filesystem -> Location -> State
craftState filesystem location = { filesystem, location }

mutate :: State -> (Filesystem -> Location -> State) -> State
mutate { filesystem, location } f = f filesystem location

mutateLocation :: State -> (Filesystem -> Location -> Location) -> State
mutateLocation { filesystem, location } f = craftState filesystem $ f filesystem location

mutateFilesystem :: State -> (Filesystem -> Location -> Filesystem) -> State
mutateFilesystem { filesystem, location } f = craftState (f filesystem location) location

setLocation :: State -> Location -> State
setLocation s loc = mutateLocation s $ \_ _ -> loc

setFilesystem :: State -> Filesystem -> State
setFilesystem s fs = mutateFilesystem s $ \_ _ -> fs