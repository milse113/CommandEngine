module Argument.Path
  ( Path
  , Relative
  , translatePath
  , translatePathToFile
  , translatePathToFolder
  ) where

import Prelude
import Argument.Argument (class Argument, parse, toString)
import Data.List (List(..), filter, fromFoldable, init, intercalate, last)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), codePointFromChar, split, uncons)
import Data.String.Utils (startsWith)
import Data.Tuple (Tuple(..), fst)
import Filesystem (FilePath, Filesystem(..), Location, emptyLocation, getFromFilesystem, mkFilePath, moveLocation)
import Main (filesystem)

data Path
  = Path Relative (List String)

instance showPath :: Show Path where
  show (Path rel strs) = "Path " <> show rel <> " " <> show strs

data Relative
  = Root
  | Up Int -- Implement
  | Current

instance showRelative :: Show Relative where
  show Root = "Root"
  show Current = "Current"
  show (Up i) = "Up " <> (show i)

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
  complete { filesystem, location } str = case folder of
    Just (Folder _ fsarr) -> map makePath $ (names fsarr)
    _ -> Nil
    where
    path :: Maybe Path
    path = (intercalate <$> Just "/" <*> (init $ fromFoldable splitStr)) >>= parse

    rel :: Maybe Relative
    rel = case path of
      Just (Path r _) -> Just r
      Nothing -> Nothing

    folder :: Maybe Filesystem
    folder = getFromFilesystem <$> Just filesystem <*> (path >>= translatePathToFolder filesystem location)

    makePath :: String -> Maybe Path
    makePath file =
      path
        >>= ( \path' -> case path' of
              (Path x y) -> pure $ Path x (y <> Cons file Nil)
          )

    names :: List Filesystem -> List String
    names fsarr =
      filter
        ( \x -> case startsWith <$> (last $ fromFoldable splitStr) <*> Just x of
            Just x' -> x'
            _ -> (1 /= 1)
        )
        $ map
            ( \x -> case x of
                Folder x _ -> x
                File x _ -> x
            )
            fsarr

    splitStr = split (Pattern "/") str
  toString (Path Current path) = Cons (intercalate "/" path) Nil
  toString (Path Root path) = Cons ("/" <> intercalate "/" path) (Cons ("~/" <> intercalate "/" path) Nil)
  toString (Path (Up _) _) = Nil -- Implement

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
