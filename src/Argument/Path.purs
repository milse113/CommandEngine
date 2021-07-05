module Argument.Path
  ( Path
  , Relative
  , translatePath
  , translatePathToFile
  , translatePathToFolder
  ) where

import Prelude
import Argument.Argument (class Argument)
import Data.List (List(..), fromFoldable, init, last)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), codePointFromChar, split, uncons)
import Data.Tuple (Tuple(..))
import Filesystem (FilePath, Filesystem, Location, emptyLocation, getFromFilesystem, mkFilePath, moveLocation)

data Path
  = Path Relative (List String)

instance showPath :: Show Path where
  show (Path rel strs) = append "Path " (append (append (show rel) " ") (show strs))

data Relative
  = Root
  | Up Int -- Implement
  | Current

instance showRelative :: Show Relative where
  show Root = "Root"
  show Current = "Current"
  show (Up i) = append "Up " (show i)

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

translatePath :: Filesystem -> Location -> Path -> Tuple (Maybe Location) (Maybe FilePath)
translatePath fs loc p = Tuple (translatePathToFolder fs loc p) (translatePathToFile fs loc p)

translatePathToFolder :: Filesystem -> Location -> Path -> Maybe Location
translatePathToFolder fs _ (Path Root path) = moveLocation fs emptyLocation path

translatePathToFolder fs loc (Path Current path) = moveLocation fs loc path

translatePathToFolder _ _ (Path (Up _) _) = Nothing -- Implement

translatePathToFile :: Filesystem -> Location -> Path -> Maybe FilePath
translatePathToFile fs _ (Path Root (Cons filename Nil)) = mkFilePath fs emptyLocation filename

translatePathToFile fs loc (Path Current (Cons filename Nil)) = mkFilePath fs loc filename

translatePathToFile fs loc (Path Current path) = join $ mkFilePath <$> Just fs <*> (init path >>= moveLocation fs loc) <*> last path
  where
  fsloc :: Maybe Filesystem
  fsloc = getFromFilesystem <$> Just fs <*> (init path >>= moveLocation fs emptyLocation)

translatePathToFile fs _ (Path Root path) = translatePathToFile fs emptyLocation (Path Current path)

translatePathToFile _ _ (Path (Up _) _) = Nothing -- Implement
