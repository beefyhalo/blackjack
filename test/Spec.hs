module Main (main) where

import Game.StateTest.StateSpec qualified as StateSpec
import Hedgehog.Main (defaultMain)

main :: IO ()
main = defaultMain [StateSpec.tests]
