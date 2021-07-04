module Commands.Echo where

import Commands.Command
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Prelude (($))

data Echo
  = Echo String

instance commandEcho :: Command Echo where
  parse x = Just $ Echo x
  run (Echo x) = x
  autocomplete _ = Nil
