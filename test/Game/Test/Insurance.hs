{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Test.Insurance (tests) where

import Control.Monad (void)
import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Domain
import Game.Gen
import Game.Insurance (decideInsurance, evolveOfferingInsurance, evolveResolvingInsurance)
import GameTopology (Game (Game, state), GameContext (..), GameState (..), InsuranceContext (..), OpeningContext (..), ResolutionContext (..))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Prelude hiding (round)

tests :: IO Bool
tests = checkParallel $$discover

-- decide emits a InsuranceTaken event
prop_decide_take_insurance_emits_PlayerTookInsurance :: Property
prop_decide_take_insurance_emits_PlayerTookInsurance = property do
  game@Game {state = OfferingInsuranceState context@GameContext {rounds}} <- forAll genOfferingInsuranceStateGame
  (pid, round) <- forAll $ Gen.element (Map.toList rounds)
  let PlayerRound {player = Player {stack = PlayerStack {chips}}} = round
      round' = round {insurance = Nothing}
      rounds' = Map.insert pid round' rounds
      game' = game {state = OfferingInsuranceState context {rounds = rounds'}}
  bet <- forAll $ genBet chips
  decideInsurance game' (TakeInsurance pid bet) === Right (PlayerTookInsurance pid bet)

-- rejects when player already insured
prop_decide_take_insurance_rejects_already_insured :: Property
prop_decide_take_insurance_rejects_already_insured = property do
  game@Game {state = OfferingInsuranceState context@GameContext {rounds}} <- forAll genOfferingInsuranceStateGame
  insuranceChoice <- forAll genInsuranceChoice
  (pid, round) <- forAll $ Gen.element (Map.toList rounds)
  let PlayerRound {player = Player {stack = PlayerStack {chips}}} = round
      round' = round {insurance = Just insuranceChoice}
      rounds' = Map.insert pid round' rounds
      game' = game {state = OfferingInsuranceState context {rounds = rounds'}}
  bet <- forAll $ genBet chips
  decideInsurance game' (TakeInsurance pid bet) === Left PlayerAlreadyInsured

-- rejects when the sidebet is malsized

-- -- decide rejects if not in the lobby

-- decide emits a InsuranceTaken event
prop_decide_reject_insurance_emits_PlayerDeclinedInsurance :: Property
prop_decide_reject_insurance_emits_PlayerDeclinedInsurance = property do
  game@Game {state = OfferingInsuranceState context@GameContext {rounds}} <- forAll genOfferingInsuranceStateGame
  (pid, round) <- forAll $ Gen.element (Map.toList rounds)
  let round' = round {insurance = Nothing}
      rounds' = Map.insert pid round' rounds
      game' = game {state = OfferingInsuranceState context {rounds = rounds'}}
  decideInsurance game' (RejectInsurance pid) === Right (PlayerDeclinedInsurance pid)

-- rejects when player already insured
prop_decide_reject_insurance_rejects_already_insured :: Property
prop_decide_reject_insurance_rejects_already_insured = property do
  game@Game {state = OfferingInsuranceState context@GameContext {rounds}} <- forAll genOfferingInsuranceStateGame
  insuranceChoice <- forAll genInsuranceChoice
  (pid, round) <- forAll $ Gen.element (Map.toList rounds)
  let round' = round {insurance = Just insuranceChoice}
      rounds' = Map.insert pid round' rounds
      game' = game {state = OfferingInsuranceState context {rounds = rounds'}}
  decideInsurance game' (RejectInsurance pid) === Left PlayerAlreadyInsured

-- decide resolve insurance state
prop_decide_offer_ResolveInsurance :: Property
prop_decide_offer_ResolveInsurance = property do
  game <- forAll genOfferingInsuranceStateGame
  decideInsurance game ResolveInsurance === Left PlayersStillBetting

-- decide resolve insurance state
prop_decide_resolve_ResolveInsurance :: Property
prop_decide_resolve_ResolveInsurance = property do
  game@Game {state = ResolvingInsuranceState GameContext {rounds}} <- forAll genResolvingInsuranceStateGame
  case decideInsurance game ResolveInsurance of
    Right (InsuranceResolved payouts) -> Map.size payouts === Map.size rounds
    decision -> annotateShow decision >> failure

-- evolve offering insurances advances the state when all players are in
prop_evolve_PlayerTookInsurance_advances_state :: Property
prop_evolve_PlayerTookInsurance_advances_state = property do
  game@Game {state = OfferingInsuranceState (GameContext _ rounds _)} <- forAll genOfferingInsuranceStateGame
  pid <- forAll $ Gen.element (Map.keys rounds)
  bet <- forAll $ genChips >>= genBet
  let evolved = evolveOfferingInsurance game (PlayerTookInsurance pid bet)
  case evolved of
    EvolutionResult Game {state = OfferingInsuranceState (GameContext _ rounds' _)} ->
      let PlayerRound {insurance} = rounds' Map.! pid
       in insurance === Just (TookInsurance bet)
    EvolutionResult Game {state = ResolvingInsuranceState (GameContext _ rounds' _)} ->
      assert $ all (isJust . insurance) rounds'
    EvolutionResult game' -> annotateShow game' >> failure

-- evolve offering insurances advances the state when all players are in
prop_evolve_PlayerDeclinedInsurance_advances_state :: Property
prop_evolve_PlayerDeclinedInsurance_advances_state = property do
  game@Game {state = OfferingInsuranceState (GameContext _ rounds _)} <- forAll genOfferingInsuranceStateGame
  pid <- forAll $ Gen.element (Map.keys rounds)
  let evolved = evolveOfferingInsurance game (PlayerDeclinedInsurance pid)
  case evolved of
    EvolutionResult Game {state = OfferingInsuranceState (GameContext _ rounds' _)} ->
      let PlayerRound {insurance} = rounds' Map.! pid
       in insurance === Just DeclinedInsurance
    EvolutionResult Game {state = ResolvingInsuranceState (GameContext _ rounds' _)} ->
      assert $ all (isJust . insurance) rounds'
    EvolutionResult game' -> annotateShow game' >> failure

prop_evolve_InsuranceResolved :: Property
prop_evolve_InsuranceResolved = property do
  game@Game {state = ResolvingInsuranceState (GameContext _ rounds _)} <- forAll genResolvingInsuranceStateGame
  payouts <- forAll $ traverse (const genInsurancePayout) rounds
  let evolved = evolveResolvingInsurance game (InsuranceResolved payouts)
  case evolved of
    EvolutionResult Game {state = OpeningTurnState OpeningContext {insuranceContext, readyPlayers}} -> do
      let InsuranceContext {context = GameContext _ rounds' _} = insuranceContext
          checkStack pid round =
            let round' = rounds Map.! pid
             in if payouts Map.! pid == NoInsurance
                  then success
                  else stack (player round) /== stack (player round')
      Map.size rounds === Map.size rounds'
      void $ Map.traverseWithKey checkStack rounds'
      assert (null readyPlayers)
    EvolutionResult Game {state = ResolvingState ResolutionContext {resolvedDealer}} ->
      cover 20 "Dealer Blackjack" (isBlackjack (dealerHand resolvedDealer))
    EvolutionResult game' -> annotateShow game' >> failure

forAllNonOfferingInsuranceStateGame :: PropertyT IO SomeGame
forAllNonOfferingInsuranceStateGame = do
  forAllWith (\(SomeGame g) -> show g) $
    Gen.filter (\case SomeGame Game {state = OfferingInsuranceState {}} -> False; _ -> True) genGame
