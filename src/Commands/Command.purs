module Commands.Command where

import Data.Maybe
import Data.List (List)

class Command a where
  parse :: String -> Maybe a
  run :: a -> String
  autocomplete :: String -> List a
