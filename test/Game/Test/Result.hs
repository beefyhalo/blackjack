{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Test.Result (tests) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Domain
import Game.Gen
import Game.Result (decideResult, evolveResult)
import GameTopology (SomeGame(SomeGame), Game (Game, state), GameState (..))
import Hedgehog
import Hedgehog.Gen qualified as Gen

tests :: IO Bool
tests = checkParallel $$discover

prop_decide_emits_GameRestarted :: Property
prop_decide_emits_GameRestarted = property do
  game <- forAll genResultStateGame
  decideResult game RestartGame === Right GameRestarted

prop_decide_rejects_non_result_state :: Property
prop_decide_rejects_non_result_state = property do
  SomeGame game <- forAllNonResultStateGame
  decideResult game RestartGame === Left GameAlreadyStarted

prop_evolve_GameRestarted :: Property
prop_evolve_GameRestarted = property do
  game@Game {state = ResultState playerMap} <- forAll genResultStateGame
  let evolved = evolveResult game GameRestarted
  case evolved of
    EvolutionResult Game {state = LobbyState playerMap'} ->
      playerMap === playerMap'
    EvolutionResult game' -> annotateShow game' >> failure

forAllNonResultStateGame :: PropertyT IO SomeGame
forAllNonResultStateGame =
  forAllWith (\(SomeGame g) -> show g) $
    Gen.filter (\case SomeGame Game {state = ResultState {}} -> False; _ -> True) genGame
