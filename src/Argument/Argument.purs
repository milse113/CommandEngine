module Argument.Argument where

import Data.List (List(..))
import Data.Maybe (Maybe(..))

class Argument a where
  parse :: String -> Maybe a
  complete :: String -> List a

instance argumentString :: Argument String where
  parse x = Just x
  complete _ = Nil
