module Main where

import Prelude

import Commands.Commands (runCommand)
import Data.List (fromFoldable)
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Filesystem (Filesystem(..))
import State (newState)

filesystem :: Filesystem
filesystem =
  Folder ""
    $ fromFoldable
      [ File "aboutme.txt" ""
      , Folder "projects"
        $ fromFoldable
          [ Folder "minecraft"
            $ fromFoldable
              [ Folder "utility_mods"
                $ fromFoldable
                  [ File "ember.txt" ""
                  , File "cosmos.txt" ""
                  ]
                , File "kyra.txt" ""
                , File "galaxy.txt" ""
              ]
            , Folder "websites"
            $ fromFoldable
              [ File "resume.txt" ""
              , File "cosmos.txt" ""
              ]
          ]
      ]

main :: Effect Unit
main = do
  let tuple = runCommand (newState filesystem) "cd proje"
  log $ show $ fst tuple
  log $ show $ snd tuple