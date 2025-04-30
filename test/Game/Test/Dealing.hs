{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Test.Dealing (tests) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Traversable (for)
import Types
import Game.Dealing (decideDealing, evolveDealing)
import Game.Gen
import GameTopology (Game (Game, state), GameContext (GameContext), GameState (..), InsuranceContext (..), OpeningContext (..), SomeGame (SomeGame))
import Hedgehog
import Hedgehog.Gen qualified as Gen

tests :: IO Bool
tests = checkParallel $$discover

-- decide emits a CardsDealt event when in dealing state
prop_decide_emits_CardsDealt :: Property
prop_decide_emits_CardsDealt = property do
  game@Game {state = DealingState players deck} <- forAll genDealingStateGame
  case decideDealing game DealInitialCards of
    Right (CardsDealt playerHands _) -> length players === length playerHands
    Left EmptyDeck ->
      let actual = drawn deck + (length players + 1) * 2
          expected = shoeSize deck * 52
       in diff actual (>) expected
    decision -> annotateShow decision >> failure

-- evolve CardsDealt advances the state to insurance of 1st player turn
prop_evolve_CardsDealt_advances_state :: Property
prop_evolve_CardsDealt_advances_state = property do
  game@Game {state = DealingState players deck} <- forAll genDealingStateGame
  playerHands <- forAll do
    playerIds <- Set.toList <$> Gen.subset (Map.keysSet players)
    for playerIds \p -> fmap (p,) genHand
  dealer <- forAll genDealer
  let evolved = evolveDealing game (CardsDealt playerHands dealer)
  case evolved of
    EvolutionResult Game {state = OfferingInsuranceState (GameContext deck' rounds dealer')} -> do
      assert (hasAce (dealerHand dealer'))
      length rounds === length playerHands
      diff (drawn deck) (<) (drawn deck')
    EvolutionResult Game {state = OpeningTurnState OpeningContext {insuranceContext, readyPlayers}} -> do
      let InsuranceContext {context = GameContext deck' rounds dealer', insurancePayouts} = insuranceContext
      assert (null readyPlayers)
      assert (null insurancePayouts)
      length rounds === length playerHands
      diff (drawn deck) (<) (drawn deck')
      dealer === dealer'
    _ -> failure

-- decide rejects commands if not in the dealing state
prop_decide_rejects_DealInitialCards_in_non_dealing_state :: Property
prop_decide_rejects_DealInitialCards_in_non_dealing_state = property do
  SomeGame game <- forAllNonDealingStateGame
  let decision = decideDealing game DealInitialCards
  case state game of
    BettingState {} -> decision === Left PlayersStillBetting
    _ -> decision === Left BadCommand

forAllNonDealingStateGame :: PropertyT IO SomeGame
forAllNonDealingStateGame = do
  forAllWith (\(SomeGame g) -> show g) $
    Gen.filter (\case SomeGame Game {state = DealingState {}} -> False; _ -> True) genGame
