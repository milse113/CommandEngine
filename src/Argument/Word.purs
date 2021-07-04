module Argument.Word
  ( Word
  ) where

import Prelude
import Argument.Argument (class Argument)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains, trim)

newtype Word
  = Word String

instance argumentWord :: Argument Word where
  parse x
    | Pattern (trim x) `contains` " " = Nothing
    | otherwise = Just $ Word x
  complete _ = Nil
