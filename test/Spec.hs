module Main (main) where

import Game.DomainSpec qualified as DomainSpec
import Game.StateTest.StateSpec qualified as StateSpec
import Hedgehog.Main (defaultMain)
import Game.Test.Lobby qualified as LobbySpec

main :: IO ()
main = defaultMain [DomainSpec.tests, LobbySpec.tests, StateSpec.tests]
