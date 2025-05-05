{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Test.Betting (tests) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Map.Strict qualified as Map
import Game.Betting (decideBetting, evolveBetting)
import Game.Gen
import GameTopology (Game (Game, state), GameState (BettingState, DealingState), SomeGame (SomeGame))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Types

tests :: IO Bool
tests = checkParallel $$discover

-- decide emits a PlaceBet event when placing a fresh bet
prop_decide_bet_emits_PlaceBet :: Property
prop_decide_bet_emits_PlaceBet = property do
  game@Game {state = BettingState players} <- forAll genBettingStateGame
  (pid, player) <- forAll $ Gen.element (Map.toList players)
  let player' = player {stack = player.stack {currentBet = 0}}
      game' = game {state = BettingState (Map.insert pid player' players)}
  bet <- forAll $ genBet player.stack.chips
  decideBetting game' (PlaceBet pid bet) === Right (BetPlaced pid bet)

-- decide rejects with a MalsizedBet

-- decide rejects if not in the lobby

-- evolve updates the state with a joined player
prop_evolve_BetPlaced_advances_state :: Property
prop_evolve_BetPlaced_advances_state = property do
  game@Game {state = BettingState players} <- forAll genBettingStateGame
  pid <- forAll (Gen.element (Map.keys players))
  chips <- forAll genChips
  bet <- forAll (genBet chips)
  let evolved = evolveBetting game (BetPlaced pid bet)
  case evolved of
    -- taking bets
    EvolutionResult Game {state = BettingState players'} ->
      let player = players' Map.! pid
       in currentBet (stack player) === bet
    -- bets locked in
    EvolutionResult Game {state = DealingState players' _} ->
      assert $ all ((> 0) . currentBet . stack) players'
    _ -> failure

forAllNonBettingStateGame :: PropertyT IO SomeGame
forAllNonBettingStateGame = do
  forAllWith (\(SomeGame g) -> show g) $
    Gen.filter (\case SomeGame Game {state = BettingState {}} -> False; _ -> True) genGame
