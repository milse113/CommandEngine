module Command.Commands.Echo where

import Command.Commands.Command
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (($))

data Echo
  = Echo String

instance commandEcho :: Command Echo where
  parseCommand x = Just $ Echo x
  run s (Echo x) = Tuple s x
  autocomplete _ = Nil
