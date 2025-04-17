{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Test.PlayerTurn (tests) where

import Data.List.NonEmpty.Zipper qualified as Z
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Domain
import Game.Gen
import Game.PlayerTurn (decidePlayerTurn)
import GameTopology (Game (Game, state), GameContext (..), GameState (OpeningTurnState, PlayerTurnState), InsuranceContext (..), OpeningContext (..))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Prelude hiding (round)

tests :: IO Bool
tests = checkParallel $$discover

prop_hit_opening_turn_draws_card :: Property
prop_hit_opening_turn_draws_card = property do
  game@Game {state = OpeningTurnState OpeningContext {insuranceContext}} <- forAll genOpeningTurnStateGame
  let InsuranceContext {context = GameContext deck rounds _} = insuranceContext
  pid <- forAll $ Gen.element (Map.keys rounds)
  (card, _) <- maybe discard pure (drawCard deck)
  decidePlayerTurn game (Hit pid) === Right (HitCard pid card)

prop_hit_player_turn_draws_card :: Property
prop_hit_player_turn_draws_card = property do
  game@Game {state = PlayerTurnState InsuranceContext {context = GameContext deck rounds _}} <- forAll genPlayerTurnStateGame
  pid <- forAll $ Gen.element (Map.keys rounds)
  (card, _) <- maybe discard pure (drawCard deck)
  decidePlayerTurn game (Hit pid) === Right (HitCard pid card)

prop_opening_turn_stand_isValid :: Property
prop_opening_turn_stand_isValid = property do
  game@Game {state = OpeningTurnState OpeningContext {insuranceContext}} <- forAll genOpeningTurnStateGame
  let InsuranceContext {context = GameContext _ rounds _} = insuranceContext
  pid <- forAll $ Gen.element (Map.keys rounds)
  decidePlayerTurn game (Stand pid) === Right (PlayerStood pid)

prop_player_turn_stand_isValid :: Property
prop_player_turn_stand_isValid = property do
  game@Game {state = PlayerTurnState InsuranceContext {context = GameContext _ rounds _}} <- forAll genPlayerTurnStateGame
  pid <- forAll $ Gen.element (Map.keys rounds)
  decidePlayerTurn game (Stand pid) === Right (PlayerStood pid)

prop_doubledown_opening_turn_isValid :: Property
prop_doubledown_opening_turn_isValid = property do
  game@Game {state = OpeningTurnState OpeningContext {insuranceContext}} <- forAll genOpeningTurnStateGame
  let InsuranceContext {context = GameContext deck rounds dealer} = insuranceContext
  (pid, round@PlayerRound {player = player@Player {stack}, hands}) <- forAll $ Gen.element (Map.toList rounds)
  (card, _) <- maybe discard pure (drawCard deck)
  let round' = round {player = player {stack = stack {chips = let Bet b = bet (Z.current hands) in b * 2}}}
      rounds' = Map.insert pid round' rounds
      readyPlayers = Set.empty
      game' = game {state = OpeningTurnState $ OpeningContext (insuranceContext {context = GameContext deck rounds' dealer}) readyPlayers}
  decidePlayerTurn game' (DoubleDown pid) === Right (PlayerDoubledDown pid card)

prop_reject_double_doubledown :: Property
prop_reject_double_doubledown = property do
  game@Game {state = OpeningTurnState OpeningContext {insuranceContext}} <- forAll genOpeningTurnStateGame
  let InsuranceContext {context = GameContext deck rounds dealer} = insuranceContext
  (pid, round@PlayerRound {player = player@Player {stack}, hands}) <- forAll $ Gen.element (Map.toList rounds)
  let round' = round {player = player {stack = stack {chips = let Bet b = bet (Z.current hands) in b * 2}}}
      rounds' = Map.insert pid round' rounds
      readyPlayers = Set.fromList [pid]
      game' = game {state = OpeningTurnState $ OpeningContext (insuranceContext {context = GameContext deck rounds' dealer}) readyPlayers}
  decidePlayerTurn game' (DoubleDown pid) === Left PlayerAlreadyActed

-- reject double down malsized bet (not enough chips)

prop_split_opening_turn_isValid :: Property
prop_split_opening_turn_isValid = property do
  game@Game {state = OpeningTurnState OpeningContext {insuranceContext}} <- forAll genOpeningTurnStateGame
  let InsuranceContext {context = GameContext deck rounds dealer} = insuranceContext
  (pid, round@PlayerRound {player = player@Player {stack}, hands}) <- forAll $ Gen.element (Map.toList rounds)
  card1@(Card rank _) <- forAll genCard
  card2 <- forAll $ Card rank <$> genSuit
  (Hand [draw2, draw1], _) <- maybe discard pure (dealN 2 deck)
  let currentHand = Z.current hands
      round' =
        round
          { hands = Z.replace (currentHand {hand = Hand [card1, card2]}) hands,
            player = player {stack = stack {chips = let Bet b = bet currentHand in b * 2}}
          }
      rounds' = Map.insert pid round' rounds
      readyPlayers = Set.empty
      game' = game {state = OpeningTurnState $ OpeningContext (insuranceContext {context = GameContext deck rounds' dealer}) readyPlayers}
  decidePlayerTurn game' (Split pid) === Right (PlayerSplitHand pid card1 card2 draw1 draw2)

-- reject split malsized bet (not enough chips)

-- reject bad split
prop_split_reject_bad_split_hand :: Property
prop_split_reject_bad_split_hand = property do
  game@Game {state = OpeningTurnState OpeningContext {insuranceContext}} <- forAll genOpeningTurnStateGame
  let InsuranceContext {context = GameContext deck rounds dealer} = insuranceContext
  (pid, round@PlayerRound {player = player@Player {stack}, hands}) <- forAll $ Gen.element (Map.toList rounds)
  card1 <- forAll genCard
  card2 <- forAll $ Gen.filter ((/= rank card1) . rank) genCard
  let currentHand = Z.current hands
      round' =
        round
          { hands = Z.replace (currentHand {hand = Hand [card1, card2]}) hands,
            player = player {stack = stack {chips = let Bet b = bet currentHand in b * 2}}
          }
      rounds' = Map.insert pid round' rounds
      readyPlayers = Set.empty
      game' = game {state = OpeningTurnState $ OpeningContext (insuranceContext {context = GameContext deck rounds' dealer}) readyPlayers}
  decidePlayerTurn game' (Split pid) === Left BadCommand

prop_surrender_opening_turn_isValid :: Property
prop_surrender_opening_turn_isValid = property do
  game@Game {state = OpeningTurnState OpeningContext {insuranceContext}} <- forAll genOpeningTurnStateGame
  let InsuranceContext {context = GameContext _ rounds _} = insuranceContext
  pid <- forAll $ Gen.element (Map.keys rounds)
  let readyPlayers = Set.empty
      game' = game {state = OpeningTurnState (OpeningContext insuranceContext readyPlayers)}
  decidePlayerTurn game' (Surrender pid) === Right (PlayerSurrendered pid)

prop_reject_double_surrender :: Property
prop_reject_double_surrender = property do
  game@Game {state = OpeningTurnState OpeningContext {insuranceContext}} <- forAll genOpeningTurnStateGame
  let InsuranceContext {context = GameContext _ rounds _} = insuranceContext
  pid <- forAll $ Gen.element (Map.keys rounds)
  let readyPlayers = Set.fromList [pid]
      game' = game {state = OpeningTurnState (OpeningContext insuranceContext readyPlayers)}
  decidePlayerTurn game' (Surrender pid) === Left PlayerAlreadyActed
