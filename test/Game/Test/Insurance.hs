{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Test.Insurance (tests) where

import Control.Monad (void)
import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Game.Gen
import Game.Insurance (decideInsurance, evolveOfferingInsurance, evolveResolvingInsurance)
import GameTopology (Game (Game, state), GameContext (..), GameState (..), InsuranceContext (..), OpeningContext (..), ResolutionContext (..), SomeGame (SomeGame))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Types
import Prelude hiding (round)

tests :: IO Bool
tests = checkParallel $$discover

-- decide emits a InsuranceTaken event
prop_decide_take_insurance_emits_PlayerTookInsurance :: Property
prop_decide_take_insurance_emits_PlayerTookInsurance = property do
  game@Game {state = OfferingInsuranceState ctx} <- forAll genOfferingInsuranceStateGame
  (pid, round) <- forAll $ Gen.element (Map.toList ctx.rounds)
  let round' = round {insurance = Nothing}
      rounds' = Map.insert pid round' ctx.rounds
      game' = game {state = OfferingInsuranceState ctx {rounds = rounds'}}
  bet <- forAll $ genBet round.player.stack.chips
  decideInsurance game' (TakeInsurance pid bet) === Right (PlayerTookInsurance pid bet)

-- rejects when player already insured
prop_decide_take_insurance_rejects_already_insured :: Property
prop_decide_take_insurance_rejects_already_insured = property do
  game@Game {state = OfferingInsuranceState context@GameContext {rounds}} <- forAll genOfferingInsuranceStateGame
  insuranceChoice <- forAll genInsuranceChoice
  (pid, round) <- forAll $ Gen.element (Map.toList rounds)
  let round' = round {insurance = Just insuranceChoice}
      rounds' = Map.insert pid round' rounds
      game' = game {state = OfferingInsuranceState context {rounds = rounds'}}
  bet <- forAll $ genBet round.player.stack.chips
  decideInsurance game' (TakeInsurance pid bet) === Left PlayerAlreadyInsured

-- rejects when the sidebet is malsized
prop_decide_take_insurance_rejects_malsized_sidebet :: Property
prop_decide_take_insurance_rejects_malsized_sidebet = property do
  game@Game {state = OfferingInsuranceState context@GameContext {rounds}} <- forAll genOfferingInsuranceStateGame
  (pid, round) <- forAll $ Gen.element (Map.toList rounds)
  let round' = round {insurance = Nothing}
      rounds' = Map.insert pid round' rounds
      game' = game {state = OfferingInsuranceState context {rounds = rounds'}}
      sidebet = Bet (round.player.stack.chips + 1)
  decideInsurance game' (TakeInsurance pid sidebet) === Left MalsizedBet

-- decide rejects if not in the insurance state
prop_decide_rejects_TakeInsurance_in_non_offering_insurance_state :: Property
prop_decide_rejects_TakeInsurance_in_non_offering_insurance_state = property do
  SomeGame game <- forAllNonOfferingInsuranceStateGame
  pid <- forAll genPlayerId
  decideInsurance game (TakeInsurance pid (Bet 1)) === Left BadCommand

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
  game@Game {state = ResolvingInsuranceState ctx} <- forAll genResolvingInsuranceStateGame
  case decideInsurance game ResolveInsurance of
    Right (InsuranceResolved payouts) -> Map.size payouts === Map.size ctx.rounds
    decision -> annotateShow decision >> failure

-- evolve offering insurances advances the state when all players are in
prop_evolve_PlayerTookInsurance_advances_state :: Property
prop_evolve_PlayerTookInsurance_advances_state = property do
  game@Game {state = OfferingInsuranceState ctx} <- forAll genOfferingInsuranceStateGame
  pid <- forAll $ Gen.element (Map.keys ctx.rounds)
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
  game@Game {state = OfferingInsuranceState ctx} <- forAll genOfferingInsuranceStateGame
  pid <- forAll $ Gen.element (Map.keys ctx.rounds)
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
  game@Game {state = ResolvingInsuranceState ctx} <- forAll genResolvingInsuranceStateGame
  payouts <- forAll $ traverse (const genInsurancePayout) ctx.rounds
  let evolved = evolveResolvingInsurance game (InsuranceResolved payouts)
  case evolved of
    EvolutionResult Game {state = OpeningTurnState ctx'} -> do
      let rounds' = ctx'.insuranceContext.context.rounds
          checkStack pid round =
            let round' = ctx.rounds Map.! pid
             in if payouts Map.! pid == NoInsurance
                  then success
                  else round.player.stack /== round'.player.stack
      Map.size ctx.rounds === Map.size rounds'
      void $ Map.traverseWithKey checkStack rounds'
      assert (null ctx'.readyPlayers)
    EvolutionResult Game {state = ResolvingState ResolutionContext {resolvedDealer}} ->
      cover 20 "Dealer Blackjack" (isBlackjack (dealerHand resolvedDealer))
    EvolutionResult game' -> annotateShow game' >> failure

forAllNonOfferingInsuranceStateGame :: PropertyT IO SomeGame
forAllNonOfferingInsuranceStateGame = do
  forAllWith (\(SomeGame g) -> show g) $
    Gen.filter (\case SomeGame Game {state = OfferingInsuranceState {}} -> False; _ -> True) genGame
