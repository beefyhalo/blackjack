module Main (main) where

import Game.DomainSpec qualified as DomainSpec
import Game.Test.Betting qualified as BettingSpec
import Game.Test.Dealing qualified as DealingSpec
import Game.Test.Insurance qualified as InsuranceSpec
import Game.Test.Lobby qualified as LobbySpec
import Game.Test.PlayerTurn qualified as PlayerTurnSpec
import Game.Test.Resolution qualified as ResolutionSpec
import Game.Test.Result qualified as ResultSpec
import Hedgehog.Main (defaultMain)

main :: IO ()
main =
  defaultMain
    [ DomainSpec.tests,
      LobbySpec.tests,
      BettingSpec.tests,
      DealingSpec.tests,
      InsuranceSpec.tests,
      PlayerTurnSpec.tests,
      ResolutionSpec.tests,
      ResultSpec.tests
    ]
