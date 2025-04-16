module Main (main) where

import Game.DomainSpec qualified as DomainSpec
import Game.Test.Betting qualified as BettingSpec
import Game.Test.Dealing qualified as DealingSpec
import Game.Test.Lobby qualified as LobbySpec
import Hedgehog.Main (defaultMain)

main :: IO ()
main =
  defaultMain
    [ DomainSpec.tests,
      LobbySpec.tests,
      BettingSpec.tests,
      DealingSpec.tests
    ]
