module Commands.Commands
  ( runCommand 
  , completeCommand
  ) where

import Prelude

import Commands.Cd (Cd)
import Commands.Command (parseCommand, run)
import Data.Array (tail, (!!))
import Data.List (List)
import Data.Map (Map, fromFoldable, keys, lookup)
import Data.Maybe (Maybe(..))
import Data.Set (filter, toUnfoldable)
import Data.String (Pattern(..), joinWith, split)
import Data.String.Utils (startsWith)
import Data.Tuple (Tuple(..))
import State (State)

commands :: Map String (State -> String -> Tuple State String)
commands = fromFoldable $
  [ Tuple "cd" $ \s x -> case join $ parseCommand <$> (joinWith <$> Just " " <*> (tail $ split (Pattern " ") x)) :: Maybe Cd of
      Just c -> run s c
      Nothing -> Tuple s "cd: invalid command"
  ]

runCommand :: State -> String -> Tuple State String
runCommand s x = case split (Pattern " ") x !! 0 of
  Just "" -> Tuple s ""
  Just x' -> case lookup x' commands of
    Just f -> f s x
    Nothing -> Tuple s $ "msh: command not found: " <> x' <> "\n"
  Nothing -> Tuple s ""

completeCommand :: String -> List String
completeCommand x = toUnfoldable $ filter (startsWith x) (keys commands)