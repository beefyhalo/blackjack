{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Test.PlayerTurn (tests) where

import Crem.Decider (EvolutionResult (..))
import Data.Foldable (toList)
import Data.List.NonEmpty.Zipper qualified as Z
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Domain
import Game.Gen
import Game.PlayerTurn (decidePlayerTurn, evolveOpeningTurn)
import GameTopology (Game (Game, state), GameContext (..), GamePhase (..), GameState (DealerTurnState, OpeningTurnState, PlayerTurnState, ResolvingState), InsuranceContext (..), OpeningContext (..), SomeGame (SomeGame))
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

prop_evolve_HitCard :: Property
prop_evolve_HitCard = property do
  (game@Game {state = OpeningTurnState OpeningContext {insuranceContext}}, pid, round) <- forAll genOpeningTurnStateUnplayedHand
  let InsuranceContext {context = GameContext deck _ _} = insuranceContext
  (card, _) <- maybe discard pure (drawCard deck)
  let evolved = evolveOpeningTurn game (HitCard pid card)
  case evolved of
    EvolutionResult Game {state = OpeningTurnState OpeningContext {insuranceContext = insuranceContext'}} -> do
      let InsuranceContext {context = GameContext deck' rounds _} = insuranceContext'
          round' = rounds Map.! pid
          cardCount = handSize $ hand (Z.current (hands round))
          cardCount' = handSize $ hand (Z.current (hands round'))
      cardCount + 1 === cardCount'
      drawn deck + 1 === drawn deck'
    EvolutionResult game' -> annotateShow game' >> failure

prop_evolve_DoubleDown :: Property
prop_evolve_DoubleDown = property do
  (game, pid, round) <- forAll genOpeningTurnStateUnplayedHand
  card <- forAll genCard
  let evolved = evolveOpeningTurn game (PlayerDoubledDown pid card)
  case evolved of
    EvolutionResult Game {state = OpeningTurnState OpeningContext {insuranceContext}} -> do
      let InsuranceContext {context = GameContext _ rounds _} = insuranceContext
          round' = rounds Map.! pid
          cardCount = sum [handSize hand | HandState {hand} <- toList (hands round)]
          cardCount' = sum [handSize hand | HandState {hand} <- toList (hands round')]
          doubleDownCount = length (filter hasDoubledDown (toList (hands round)))
          doubleDownCount' = length (filter hasDoubledDown (toList (hands round')))
      annotateShow round'
      cardCount + 1 === cardCount'
      doubleDownCount + 1 === doubleDownCount'
    EvolutionResult game''@Game {state = PlayerTurnState {}} ->
      footnote "Game shouldn't be ready as we inserted a new hand" >> annotateShow game'' >> failure
    EvolutionResult game''@Game {state = DealerTurnState {}} ->
      footnote "Game shouldn't be ready as we inserted a new hand" >> annotateShow game'' >> failure
    EvolutionResult Game {state = ResolvingState {}} -> do
      footnote "If the game is resolved, everyone must have lost. Which should be impossible for newHandState is a 2 card hand"
      failure
    EvolutionResult game'' -> annotateShow game'' >> failure

-- test that readyPlayers increases

-- replace the current focus with an unplayed hand and add a new unplayed hand
genOpeningTurnStateUnplayedHand :: Gen (Game OpeningTurn, PlayerId, PlayerRound)
genOpeningTurnStateUnplayedHand = do
  game@Game {state = OpeningTurnState OpeningContext {insuranceContext}} <- genOpeningTurnStateGame
  let InsuranceContext {context = GameContext _ rounds _} = insuranceContext
  (pid, round) <- Gen.element (Map.toList rounds)
  newHandState <- genUnplayedHand
  let currentHand = Z.current (hands round)
      currentHand' = currentHand {hasDoubledDown = False, hasStood = False}
      hands' = Z.push newHandState (Z.replace currentHand' $ hands round)
      round' = round {hands = hands', hasSurrendered = False}
      rounds' = Map.insert pid round' rounds
      insuranceContext' = insuranceContext {context = (context insuranceContext) {rounds = rounds'}}
      readyPlayers = Set.empty
      game' = game {state = OpeningTurnState (OpeningContext insuranceContext' readyPlayers)}
  pure (game', pid, round')
  where
    genUnplayedHand =
      HandState
        <$> genTwoCardHand
        <*> genBet 1000
        <*> Gen.constant False
        <*> Gen.constant False
