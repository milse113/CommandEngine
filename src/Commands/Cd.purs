module Command.Commands.Cd
  ( Cd
  ) where

import Prelude

import Command.Argument.Argument (parse)
import Command.Argument.Path (Path, translatePathToFolder, validatePath)
import Command.Commands.Command (class Command)
import Data.Array (index, length)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Command.State (setLocation)

data Cd
  = Cd Path

instance commandCd :: Command Cd where
  parseCommand = pc
    where
      pc x 
        | length (list x) > 1 = Nothing
        | (list x) `index` 0 == Just "" = pc "~"
        | otherwise = (list x) `index` 0 >>= parse >>= (Just <<< Cd)

      list x = split (Pattern " ") x
  run s (Cd path) = case translatePathToFolder s path of 
    Just x -> Tuple (s `setLocation` x) "\n"
    Nothing -> Tuple s $ "cd: no such file or directory: " <> (unsafePartial $ unwrapMaybe $ validatePath s path)
    where 
      unwrapMaybe :: forall a. Partial => Maybe a -> a
      unwrapMaybe (Just x) = x
  autocomplete _ = Nil
