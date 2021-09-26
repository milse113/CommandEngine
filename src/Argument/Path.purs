module Command.Argument.Path
  ( Path
  , Relative
  , translatePath
  , translatePathToFile
  , translatePathToFolder
  , validatePath
  ) where

import Prelude

import Command.Argument.Argument (class Argument, parse)
import Data.List (List(..), catMaybes, filter, fromFoldable, init, intercalate, last)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), codePointFromChar, split, uncons)
import Data.String.Utils (startsWith)
import Data.Tuple (Tuple(..))
import Command.Filesystem (FilePath, Filesystem(..), Location, emptyLocation, getFromFilesystem, mkFilePath, moveLocation)
import Command.State (State, craftState, newState)

data Path
  = Path Relative (List String)

instance showPath :: Show Path where
  show (Path rel strs) = "Path " <> show rel <> " " <> show strs

data Relative
  = Root
  | Home
  | Up Int -- Implement
  | Current

instance showRelative :: Show Relative where
  show Root = "Root"
  show Current = "Current"
  show Home = "Home"
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
          | head' == codePointFromChar '/' -> Just $ Path Home (fromFoldable $ split (Pattern "/") tail')
        _ -> Nothing
    Nothing -> Nothing
    Just _ -> Just $ Path Current (fromFoldable $ split (Pattern "/") x)
  complete { filesystem, location } str = case folder of
    Just (Folder _ fsarr) -> catMaybes $ map makePath $ (names fsarr)
    _ -> Nil
    where
    path :: Maybe Path
    path = (intercalate <$> Just "/" <*> (init $ fromFoldable splitStr)) >>= parse

    folder :: Maybe Filesystem
    folder = getFromFilesystem <$> Just filesystem <*> (path >>= translatePathToFolder (craftState filesystem location))

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
                Folder x' _ -> x'
                File x' _ -> x'
            )
            fsarr

    splitStr = split (Pattern "/") str
  toString (Path Current path) = Cons (intercalate "/" path) Nil
  toString (Path Root path) = Cons ("/" <> intercalate "/" path) Nil
  toString (Path Home path) = Cons ("~/" <> intercalate "/" path) Nil
  toString (Path (Up _) _) = Nil -- Implement

translatePath :: State -> Path -> Tuple (Maybe Location) (Maybe FilePath)
translatePath state p = Tuple (translatePathToFolder state p) (translatePathToFile state p)

translatePathToFolder :: State -> Path -> Maybe Location
translatePathToFolder { filesystem: fs, location: _ } (Path Root path) = moveLocation fs emptyLocation path
translatePathToFolder { filesystem: fs, location: _ } (Path Home path) = moveLocation fs emptyLocation path

translatePathToFolder { filesystem: fs, location: loc } (Path Current path) = moveLocation fs loc path

translatePathToFolder _ (Path (Up _) _) = Nothing -- Implement

translatePathToFile :: State -> Path -> Maybe FilePath
translatePathToFile { filesystem: fs, location: _ } (Path Root (Cons filename Nil)) = mkFilePath fs emptyLocation filename
translatePathToFile { filesystem: fs, location: _ } (Path Home (Cons filename Nil)) = mkFilePath fs emptyLocation filename

translatePathToFile { filesystem: fs, location: loc } (Path Current (Cons filename Nil)) = mkFilePath fs loc filename

translatePathToFile { filesystem: fs, location: loc } (Path Current path) = join $ mkFilePath <$> Just fs <*> (init path >>= moveLocation fs loc) <*> last path

translatePathToFile { filesystem: fs, location: _ } (Path Root path) = translatePathToFile (newState fs) (Path Current path)
translatePathToFile { filesystem: fs, location: _ } (Path Home path) = translatePathToFile (newState fs) (Path Current path)

translatePathToFile _ (Path (Up _) _) = Nothing -- Implement

validatePath :: State -> Path -> Maybe String
validatePath _ (Path _ Nil) = Nothing
validatePath { filesystem: filesystem, location: _ } (Path Root (Cons f fs)) = case moveLocation filesystem emptyLocation (Cons f Nil) of
  Just loc -> validatePath (craftState filesystem loc) (Path Current fs)
  Nothing -> Just f
validatePath { filesystem: filesystem, location: location } (Path Current (Cons f fs)) = case moveLocation filesystem location (Cons f Nil) of
  Just loc -> validatePath (craftState filesystem loc) (Path Current fs)
  Nothing -> Just f
validatePath { filesystem: filesystem, location: location } (Path Home (Cons f fs)) = case moveLocation filesystem location (Cons f Nil) of
  Just loc -> validatePath (craftState filesystem loc) (Path Current fs)
  Nothing -> Just f
validatePath _ (Path (Up _) _) = Nothing -- Implement
