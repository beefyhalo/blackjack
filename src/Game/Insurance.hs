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

decideTakeInsurance :: PlayerId -> Bet -> Game vertex -> Decision
decideTakeInsurance pid sidebet = \case
  Game {state = OfferingInsuranceState GameContext {sessions}} ->
    withSession pid sessions \PlayerSession {player = Player {stack}, insurance} ->
      if isJust insurance
        then Left PlayerAlreadyInsured
        else withValidBet sidebet (chips stack) (Right . PlayerTookInsurance pid . current)
  _ -> Left BadCommand

decideRejectInsurance :: PlayerId -> Game vertex -> Decision
decideRejectInsurance pid = \case
  Game {state = OfferingInsuranceState GameContext {sessions}} ->
    withSession pid sessions \PlayerSession {insurance} ->
      if isJust insurance
        then Left PlayerAlreadyInsured
        else Right (PlayerDeclinedInsurance pid)
  _ -> Left BadCommand

decideResolveInsurance :: Game vertex -> Decision
decideResolveInsurance = \case
  Game {state = ResolvingInsuranceState GameContext {sessions, dealer}} ->
    let insurancePayouts = fmap (payoutForInsurance dealer) sessions
     in Right (InsuranceResolved insurancePayouts)
  _ -> Left BadCommand

evolveOfferingInsurance :: Game OfferingInsurance -> Event -> EvolutionResult GameTopology Game OfferingInsurance output
evolveOfferingInsurance game@Game {state = OfferingInsuranceState context@GameContext {sessions}} = \case
  PlayerTookInsurance pid chips ->
    let sessions' = Map.adjust (\s -> s {insurance = Just (TookInsurance chips)}) pid sessions
     in advanceState sessions'
  PlayerDeclinedInsurance pid ->
    let sessions' = Map.adjust (\s -> s {insurance = Just DeclinedInsurance}) pid sessions
     in advanceState sessions'
  _ -> EvolutionResult game
  where
    advanceState sessions'
      | all (isJust . insurance) sessions' = EvolutionResult game {state = ResolvingInsuranceState context {sessions = sessions'}}
      | otherwise = EvolutionResult game {state = OfferingInsuranceState context {sessions = sessions'}}

evolveResolvingInsurance :: Game ResolvingInsurance -> Event -> EvolutionResult GameTopology Game ResolvingInsurance output
evolveResolvingInsurance game@Game {state = ResolvingInsuranceState context@GameContext {sessions, dealer}} = \case
  InsuranceResolved results ->
    let sessions' = Map.mapWithKey settleInsurance results
        openingContext = OpeningContext (InsuranceContext context {sessions = sessions'} results) Set.empty
        Dealer dealerHand = dealer
     in if isBlackjack dealerHand
          then EvolutionResult game {state = ResolvingState (ResolutionContext sessions' dealer results)}
          else EvolutionResult game {state = OpeningTurnState openingContext}
  _ -> EvolutionResult game
  where
    settleInsurance :: PlayerId -> InsurancePayout -> PlayerSession
    settleInsurance pid result =
      let session = sessions Map.! pid
          Player {stack} = player session
          delta = case result of
            WonInsurancePayout amt -> amt
            LostInsuranceBet amt -> -amt
            NoInsurance -> 0
          player' = (player session) {stack = stack {chips = chips stack + delta}}
       in session {player = player'}
