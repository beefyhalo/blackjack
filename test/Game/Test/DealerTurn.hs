{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Test.DealerTurn (tests) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Either (isLeft)
import Game.DealerTurn (decideDealerPlay, evolveDealerTurn)
import Game.Gen
import GameTopology (Game (Game, state), GameContext (..), GameState (..), InsuranceContext (..), ResolutionContext (..), SomeGame (SomeGame))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Types

tests :: IO Bool
tests = checkParallel $$discover

prop_decideDealerPlay_plays_until_stand :: Property
prop_decideDealerPlay_plays_until_stand = property do
  game@Game {state = DealerTurnState ctx} <- forAll genDealerTurnStateGame
  case decideDealerPlay game DealerPlay of
    Right (DealerPlayed dealer) -> do
      assert . not $ dealerShouldHit dealer
      diff (handSize ctx.context.dealer.dealerHand) (<=) (handSize dealer.dealerHand)
    _ -> failure

prop_decideDealerPlay_no_draw_if_standing :: Property
prop_decideDealerPlay_no_draw_if_standing = property do
  dealer <- forAll genStandingDealer
  game@Game {state = DealerTurnState ctx} <- forAll genDealerTurnStateGame
  let game' = game {state = DealerTurnState ctx {context = ctx.context {dealer}}}
  case decideDealerPlay game' DealerPlay of
    Right (DealerPlayed dealer') -> dealer' === dealer
    _ -> failure
  where
    genStandingDealer = Gen.filter (not . dealerShouldHit) (fmap Dealer genTwoCardHand)

prop_decideDealerPlay_fails_if_players_still_playing :: Property
prop_decideDealerPlay_fails_if_players_still_playing = property do
  game <- forAll genPlayerTurnStateGame
  decideDealerPlay game DealerPlay === Left PlayersStillPlaying

prop_decideDealerPlay_fails_if_wrong_phase :: Property
prop_decideDealerPlay_fails_if_wrong_phase = property $ do
  SomeGame game <- forAllNonDealerTurnStateGame
  assert $ isLeft (decideDealerPlay game DealerPlay)

prop_evolveDealerTurn_preserves_rounds_and_insurance :: Property
prop_evolveDealerTurn_preserves_rounds_and_insurance = property $ do
  game@Game {state = DealerTurnState ctx} <- forAll genDealerTurnStateGame
  dealer <- forAll genDealer
  let evolved = evolveDealerTurn game (DealerPlayed dealer)
  case evolved of
    EvolutionResult Game {state = ResolvingState ctx'} -> do
      ctx.context.rounds === ctx'.resolvedRounds
      ctx.insurancePayouts === ctx'.resolvedInsurancePayouts
      dealer === ctx'.resolvedDealer
    _ -> failure

forAllNonDealerTurnStateGame :: PropertyT IO SomeGame
forAllNonDealerTurnStateGame = do
  forAllWith (\(SomeGame g) -> show g) $
    Gen.filter (\case SomeGame Game {state = DealerTurnState {}} -> False; _ -> True) genGame
