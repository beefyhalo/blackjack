{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game.Insurance
  ( decideTakeInsurance,
    decideRejectInsurance,
    decideResolveInsurance,
    evolveOfferingInsurance,
    evolveResolvingInsurance,
  )
where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Domain
import GameTopology
import Prelude hiding (round)

decideTakeInsurance :: PlayerId -> Bet -> Game vertex -> Decision
decideTakeInsurance pid sidebet = \case
  Game {state = OfferingInsuranceState GameContext {rounds}} ->
    withPlayerRound pid rounds \PlayerRound {player = Player {stack}, insurance} ->
      if isJust insurance
        then Left PlayerAlreadyInsured
        else withValidBet sidebet (chips stack) (Right . PlayerTookInsurance pid . current)
  _ -> Left BadCommand

decideRejectInsurance :: PlayerId -> Game vertex -> Decision
decideRejectInsurance pid = \case
  Game {state = OfferingInsuranceState GameContext {rounds}} ->
    withPlayerRound pid rounds \PlayerRound {insurance} ->
      if isJust insurance
        then Left PlayerAlreadyInsured
        else Right (PlayerDeclinedInsurance pid)
  _ -> Left BadCommand

decideResolveInsurance :: Game vertex -> Decision
decideResolveInsurance = \case
  Game {state = ResolvingInsuranceState GameContext {rounds, dealer}} ->
    let insurancePayouts = fmap (payoutForInsurance dealer) rounds
     in Right (InsuranceResolved insurancePayouts)
  _ -> Left BadCommand

evolveOfferingInsurance :: Game OfferingInsurance -> Event -> EvolutionResult GameTopology Game OfferingInsurance output
evolveOfferingInsurance game@Game {state = OfferingInsuranceState context@GameContext {rounds}} = \case
  PlayerTookInsurance pid chips ->
    let rounds' = Map.adjust (\r -> r {insurance = Just (TookInsurance chips)}) pid rounds
     in advanceState rounds'
  PlayerDeclinedInsurance pid ->
    let rounds' = Map.adjust (\r -> r {insurance = Just DeclinedInsurance}) pid rounds
     in advanceState rounds'
  _ -> EvolutionResult game
  where
    advanceState rounds'
      | all (isJust . insurance) rounds' = EvolutionResult game {state = ResolvingInsuranceState context {rounds = rounds'}}
      | otherwise = EvolutionResult game {state = OfferingInsuranceState context {rounds = rounds'}}

evolveResolvingInsurance :: Game ResolvingInsurance -> Event -> EvolutionResult GameTopology Game ResolvingInsurance output
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
