{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game.Insurance (decideInsurance, evolveOfferingInsurance, evolveResolvingInsurance) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Domain
import GameTopology
import Prelude hiding (round)

decideInsurance :: Game phase -> InsuranceCommand -> Decision
decideInsurance = \case
  Game {state = OfferingInsuranceState GameContext {rounds, dealer}} -> \case
    TakeInsurance pid sidebet ->
      withPlayerRound pid rounds \PlayerRound {player = Player {stack}, insurance} ->
        if isJust insurance
          then Left PlayerAlreadyInsured
          else withValidBet sidebet (chips stack) (Right . InsuranceEvt . PlayerTookInsurance pid)
    RejectInsurance pid ->
      withPlayerRound pid rounds \PlayerRound {insurance} ->
        if isJust insurance
          then Left PlayerAlreadyInsured
          else Right (InsuranceEvt $ PlayerDeclinedInsurance pid)
    ResolveInsurance ->
      let insurancePayouts = fmap (payoutForInsurance dealer) rounds
       in Right (InsuranceEvt $ InsuranceResolved insurancePayouts)
  _ -> \_ -> Left BadCommand

evolveOfferingInsurance :: Game OfferingInsurance -> InsuranceEvent -> EvolutionResult GameTopology Game OfferingInsurance output
evolveOfferingInsurance game@Game {state = OfferingInsuranceState context@GameContext {rounds}} = \case
  PlayerTookInsurance pid bet ->
    let rounds' = Map.adjust (\r -> r {insurance = Just (TookInsurance bet)}) pid rounds
     in advanceState rounds'
  PlayerDeclinedInsurance pid ->
    let rounds' = Map.adjust (\r -> r {insurance = Just DeclinedInsurance}) pid rounds
     in advanceState rounds'
  _ -> EvolutionResult game
  where
    advanceState rounds'
      | all (isJust . insurance) rounds' = EvolutionResult game {state = ResolvingInsuranceState context {rounds = rounds'}}
      | otherwise = EvolutionResult game {state = OfferingInsuranceState context {rounds = rounds'}}

evolveResolvingInsurance :: Game ResolvingInsurance -> InsuranceEvent -> EvolutionResult GameTopology Game ResolvingInsurance output
evolveResolvingInsurance game@Game {state = ResolvingInsuranceState context@GameContext {rounds, dealer}} = \case
  InsuranceResolved results ->
    let rounds' = Map.mapWithKey settleInsurance results
        openingContext = OpeningContext (InsuranceContext context {rounds = rounds'} results) Set.empty
        Dealer dealerHand = dealer
     in if isBlackjack dealerHand
          then EvolutionResult game {state = ResolvingState (ResolutionContext rounds' dealer results)}
          else EvolutionResult game {state = OpeningTurnState openingContext}
  _ -> EvolutionResult game
  where
    settleInsurance :: PlayerId -> InsurancePayout -> PlayerRound
    settleInsurance pid result =
      let round = rounds Map.! pid
          Player {stack} = player round
          delta = case result of
            WonInsurancePayout amt -> amt
            LostInsuranceBet amt -> -amt
            NoInsurance -> 0
          player' = (player round) {stack = stack {chips = chips stack + delta}}
       in round {player = player'}
