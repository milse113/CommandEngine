module Filesystem
  ( Filesystem(..)
  , Location
  , FilePath
  , emptyLocation
  , moveLocation
  , mkFilePath
  , getFromFilesystem
  , getFile
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.List (List(..), snoc)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), uncurry)
import Partial.Unsafe (unsafePartial)
import Prelude (class Show, append, otherwise, show, ($), (==), (>>=))

data Filesystem
  = Folder String (List Filesystem) -- Name Children
  | File String String -- Name Contents

instance filesystemShow :: Show Filesystem where
  show (Folder name fss) = append (append (append "Folder " $ show name) " ") $ show fss
  show (File name _) = (append "File " $ show name)

newtype Location
  = Location (List String)

data FilePath
  = FilePath Location String

instance locationShow :: Show Location where
  show (Location strs) = append "Location " $ show strs

instance filePathShow :: Show FilePath where
  show (FilePath loc file) = append (append (append "FilePath " $ show loc) " ") file

emptyLocation :: Location
emptyLocation = Location Nil

moveLocation :: Filesystem -> Location -> List String -> Maybe Location
moveLocation _ loc Nil = Just loc

moveLocation (File _ _) _ _ = Nothing

moveLocation (Folder _ Nil) _ _ = Nothing

moveLocation fs loc (Cons m ms) = moveLocation' fs loc m >>= uncurry (\loc' fs' -> moveLocation fs' loc' ms)

moveLocation' :: Filesystem -> Location -> String -> Maybe (Tuple Location Filesystem)
moveLocation' (File _ _) _ _ = Nothing

moveLocation' (Folder _ Nil) _ _ = Nothing

moveLocation' (Folder _ (Cons item files)) (Location loc) move = case item of
  File _ _ -> moveLocation' (Folder "" files) (Location loc) move
  f@(Folder name _) -> isOrRecurse name move f
  where
  isOrRecurse name' move' f
    | name' == move' = Just $ Tuple (Location $ snoc loc name') f
    | otherwise = moveLocation' (Folder "" files) (Location loc) move

getFromFilesystem :: Filesystem -> Location -> Filesystem
getFromFilesystem x y = unsafePartial (partialGff x y)
  where
  partialGff :: Partial => Filesystem -> Location -> Filesystem
  partialGff files (Location Nil) = files

  partialGff (Folder _ files) (Location (Cons loc locs)) =
    if locs == Nil then
      gff files loc
    else
      partialGff (gff files loc) $ Location locs

  gff :: Partial => List Filesystem -> String -> Filesystem
  gff (Cons file'@(Folder name _) files') loc'
    | name == loc' = file'
    | otherwise = gff files' loc'

  gff (Cons (File _ _) files') loc' = gff files' loc'

mkFilePath :: Filesystem -> Location -> String -> Maybe FilePath
mkFilePath files loc filename = FilePath <$> Just loc <*> fsHasToMaybe fs filename
  where
  fs = getFromFilesystem files loc

  fsHasToMaybe fs' s = if fsHasFile fs' s then Just s else Nothing

  fsHasFile :: Filesystem -> String -> Boolean
  fsHasFile (File _ _) _ = 1 == 2 -- I know this is weird but spago doesnt let me import boolean literals so

  fsHasFile (Folder _ Nil) _ = 1 == 2

  fsHasFile (Folder _ (Cons (File name _) fs')) n = if name == n then true else fsHasFile (Folder "" fs') n

  fsHasFile (Folder _ (Cons (Folder _ _) fs')) n = fsHasFile (Folder "" fs') n

getFile :: Filesystem -> FilePath -> Filesystem
getFile fs' path' = unsafePartial $ getFile' fs' path'
  where
  getFile' :: Partial => Filesystem -> FilePath -> Filesystem
  getFile' fs (FilePath loc filename) = getFileFromFolder (getFromFilesystem fs loc) filename

  getFileFromFolder :: Partial => Filesystem -> String -> Filesystem
  getFileFromFolder (Folder _ (Cons file@(File name _) files)) n = if name == n then file else getFileFromFolder (Folder "" files) n

  getFileFromFolder (Folder _ (Cons (Folder _ _) files)) n = getFileFromFolder (Folder "" files) n
