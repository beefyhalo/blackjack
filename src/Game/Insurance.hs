{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Game.Insurance (decideInsurance, evolveOfferingInsurance, evolveResolvingInsurance) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import GameTopology
import Types
import Prelude hiding (round)

decideInsurance :: Game phase -> InsuranceCommand -> Either GameError InsuranceEvent
decideInsurance Game {state = OfferingInsuranceState context} = \case
  TakeInsurance pid sidebet ->
    withPlayerRound pid context.rounds \round ->
      if isJust round.insurance
        then Left PlayerAlreadyInsured
        else withValidBet sidebet round.player.stack.chips (Right . PlayerTookInsurance pid)
  RejectInsurance pid ->
    withPlayerRound pid context.rounds \round ->
      if isJust round.insurance
        then Left PlayerAlreadyInsured
        else Right (PlayerDeclinedInsurance pid)
  ResolveInsurance -> Left PlayersStillBetting
-- accept the ResolveInsurance command
decideInsurance Game {state = ResolvingInsuranceState context} = \case
  ResolveInsurance ->
    let insurancePayouts = fmap (payoutForInsurance context.dealer) context.rounds
     in Right (InsuranceResolved insurancePayouts)
  _ -> Left BadCommand
decideInsurance _ = \_ -> Left BadCommand

evolveOfferingInsurance :: Game OfferingInsurance -> InsuranceEvent -> EvolutionResult GameTopology Game OfferingInsurance output
evolveOfferingInsurance game@Game {state = OfferingInsuranceState context} = \case
  PlayerTookInsurance pid bet ->
    let rounds = Map.adjust (\r -> r {insurance = Just (TookInsurance bet)}) pid context.rounds
     in advanceState rounds
  PlayerDeclinedInsurance pid ->
    let rounds = Map.adjust (\r -> r {insurance = Just DeclinedInsurance}) pid context.rounds
     in advanceState rounds
  _ -> EvolutionResult game
  where
    advanceState rounds
      | all (isJust . insurance) rounds = EvolutionResult game {state = ResolvingInsuranceState context {rounds}}
      | otherwise = EvolutionResult game {state = OfferingInsuranceState context {rounds}}

evolveResolvingInsurance :: Game ResolvingInsurance -> InsuranceEvent -> EvolutionResult GameTopology Game ResolvingInsurance output
evolveResolvingInsurance game@Game {state = ResolvingInsuranceState context} = \case
  InsuranceResolved payouts ->
    let rounds = Map.mapWithKey settleInsurance payouts
        openingContext = OpeningContext (InsuranceContext context {rounds} payouts) Set.empty
     in if isBlackjack context.dealer.dealerHand
          then EvolutionResult game {state = ResolvingState (ResolutionContext rounds context.dealer payouts)}
          else EvolutionResult game {state = OpeningTurnState openingContext}
  _ -> EvolutionResult game
  where
    settleInsurance :: PlayerId -> InsurancePayout -> PlayerRound
    settleInsurance pid result =
      let round = context.rounds Map.! pid
          player = round.player
          delta = insuranceDelta result
          player' = player {stack = player.stack {chips = player.stack.chips + delta}}
       in round {player = player'}
