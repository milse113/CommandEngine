module Commands.Command where

import Data.Maybe
import Data.List (List)
import Data.Tuple (Tuple)
import Main (State)

class Command a where
  parse :: String -> Maybe a
  run :: State -> a -> Tuple State String
  autocomplete :: String -> List a
