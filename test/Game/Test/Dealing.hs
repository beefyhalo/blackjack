{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Test.Dealing (tests) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Traversable (for)
import Game.Dealing (decideDealing, evolveDealing)
import Game.Gen
import GameTopology (Game (Game, state), GameContext (..), GameState (..), InsuranceContext (..), OpeningContext (..), SomeGame (SomeGame))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Types

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
    EvolutionResult Game {state = OfferingInsuranceState ctx} -> do
      assert (hasAce ctx.dealer.dealerHand)
      length ctx.rounds === length playerHands
      diff deck.drawn (<) ctx.deck.drawn
    EvolutionResult Game {state = OpeningTurnState ctx} -> do
      let InsuranceContext {context, insurancePayouts} = ctx.insuranceContext
      assert (null ctx.readyPlayers)
      assert (null insurancePayouts)
      length context.rounds === length playerHands
      diff deck.drawn (<) context.deck.drawn
      dealer === context.dealer
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
