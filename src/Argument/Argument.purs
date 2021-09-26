module Command.Argument.Argument where

import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Command.State (State)

class Argument a where
  parse :: String -> Maybe a
  complete :: State -> String -> List a
  toString :: a -> List String

instance argumentString :: Argument String where
  parse x = Just x
  complete _ _ = Nil
  toString x = Cons x Nil
