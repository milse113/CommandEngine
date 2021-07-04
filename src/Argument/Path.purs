module Argument.Path
  ( Path
  , Relative
  , pathToLocation
  ) where

import Prelude
import Argument.Argument (class Argument)
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), codePointFromChar, split, uncons)
import Filesystem (Filesystem, Location, emptyLocation, moveLocation)

data Path
  = Path Relative (List String)

data Relative
  = Root
  | Up Int -- Implement
  | Current

instance argumentPath :: Argument Path where
  parse "/" = Just $ Path Root Nil
  parse "~" = Just $ Path Root Nil
  parse x = case uncons x of
    Just { head, tail }
      | head == codePointFromChar '/' -> Just $ Path Root (fromFoldable $ split (Pattern "/") tail)
    Just { head, tail }
      | head == codePointFromChar '~' -> case uncons tail of
        Just { head: head', tail: tail' }
          | head' == codePointFromChar '/' -> Just $ Path Root (fromFoldable $ split (Pattern "/") tail')
        _ -> Nothing
    Nothing -> Nothing
    Just _ -> Just $ Path Current (fromFoldable $ split (Pattern "/") x)
  complete _ = Nil

pathToLocation :: Filesystem -> Location -> Path -> Maybe Location
pathToLocation fs _ (Path Root path) = moveLocation fs emptyLocation path

pathToLocation fs loc (Path Current path) = moveLocation fs loc path

pathToLocation _ _ (Path (Up _) _) = Nothing -- Implement
