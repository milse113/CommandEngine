module Main where

import Prelude
import Data.List (fromFoldable)
import Effect (Effect)
import Effect.Console (log)
import Filesystem (Filesystem(..), Location)

type State
  = { filesystem :: Filesystem
    , location :: Location
    }

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
  log "🍝"
