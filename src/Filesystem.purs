module Filesystem
  ( Filesystem(..)
  , Location
  , emptyLocation
  , moveLocation
  , getFromFilesystem
  ) where

import Prelude (class Show, append, otherwise, show, ($), (==), (>>=))
import Data.List (List(..), snoc)
import Data.Tuple (Tuple(..), uncurry)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)

data Filesystem
  = Folder String (List Filesystem) -- Name Children
  | File String String -- Name Contents

instance filesystemShow :: Show Filesystem where
  show (Folder name fss) = append (append (append "Folder " $ show name) " ") $ show fss
  show (File name _) = (append "File " $ show name)

newtype Location
  = Location (List String)

instance locationShow :: Show Location where
  show (Location strs) = append "Location " $ show strs

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
