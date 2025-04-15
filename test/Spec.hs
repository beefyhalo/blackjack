module Main (main) where

import Game.DomainSpec qualified as DomainSpec
import Game.StateTest.StateSpec qualified as StateSpec
import Hedgehog.Main (defaultMain)

main :: IO ()
main = defaultMain [DomainSpec.tests, StateSpec.tests]
