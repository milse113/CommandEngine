module Commands.Command where

import Data.List (List)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import State (State)

class Command a where
  parseCommand :: String -> Maybe a
  run :: State -> a -> Tuple State String
  autocomplete :: String -> List a
