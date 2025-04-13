{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

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

decideTakeInsurance :: PlayerId -> Chips -> Game vertex -> Decision
decideTakeInsurance pid insuranceChips = \case
  Game {state = OfferingInsuranceState GameContext {players}} ->
    withPlayer pid players \Player {playerSeat = PlayerSeat {stack}, insurance} ->
      if
        | isJust insurance -> Left PlayerAlreadyInsured
        | insuranceChips <= 0 || insuranceChips > chips stack -> Left MalsizedBet
        | otherwise -> Right (PlayerTookInsurance pid insuranceChips)
  _ -> Left BadCommand

decideRejectInsurance :: PlayerId -> Game vertex -> Decision
decideRejectInsurance pid = \case
  Game {state = OfferingInsuranceState GameContext {players}} ->
    withPlayer pid players \Player {insurance} ->
      if isJust insurance then Left PlayerAlreadyInsured else Right (PlayerDeclinedInsurance pid)
  _ -> Left BadCommand

decideResolveInsurance :: Game vertex -> Decision
decideResolveInsurance = \case
  Game {state = ResolvingInsuranceState GameContext {players, dealer = Dealer dealerHand}} ->
    let isDealerBlackjack = isBlackjack dealerHand
        insurancePayouts = fmap (payoutForInsurance isDealerBlackjack) players
     in Right (InsuranceResolved insurancePayouts)
  _ -> Left BadCommand
  where
    payoutForInsurance :: Bool -> Player -> InsurancePayout
    payoutForInsurance hasBJ Player {insurance} = case insurance of
      Just (TookInsurance amt)
        | hasBJ -> WonInsurancePayout (amt * 2) -- 2:1 insurance payout if the dealer has a blackjack
        | otherwise -> LostInsuranceBet amt
      _ -> NoInsurance

evolveOfferingInsurance :: Game OfferingInsurance -> Event -> EvolutionResult GameTopology Game OfferingInsurance output
evolveOfferingInsurance game@Game {state = OfferingInsuranceState context@GameContext {players}} = \case
  PlayerTookInsurance pid chips ->
    let players' = Map.adjust (\p -> p {insurance = Just (TookInsurance chips)}) pid players
     in advanceState players'
  PlayerDeclinedInsurance pid ->
    let players' = Map.adjust (\p -> p {insurance = Just DeclinedInsurance}) pid players
     in advanceState players'
  _ -> EvolutionResult game
  where
    advanceState players'
      | all (isJust . insurance) players' = EvolutionResult game {state = ResolvingInsuranceState context {players = players'}}
      | otherwise = EvolutionResult game {state = OfferingInsuranceState context {players = players'}}

evolveResolvingInsurance :: Game ResolvingInsurance -> Event -> EvolutionResult GameTopology Game ResolvingInsurance output
evolveResolvingInsurance game@Game {state = ResolvingInsuranceState context@GameContext {players, dealer}} = \case
  InsuranceResolved results ->
    let players' = Map.mapWithKey settleInsurance results
        openingContext = OpeningContext (InsuranceContext context {players = players'} results) Set.empty
        Dealer dealerHand = dealer
     in if isBlackjack dealerHand
          then EvolutionResult game {state = ResolvingState (ResolutionContext players' dealer results)}
          else EvolutionResult game {state = OpeningTurnState openingContext}
  _ -> EvolutionResult game
  where
    settleInsurance :: PlayerId -> InsurancePayout -> Player
    settleInsurance pid result =
      let player = players Map.! pid
          PlayerSeat {stack} = playerSeat player
          delta = case result of
            WonInsurancePayout amt -> amt
            LostInsuranceBet amt -> -amt
            NoInsurance -> 0
          playerSeat' = (playerSeat player) {stack = stack {chips = chips stack + delta}}
       in player {playerSeat = playerSeat'}
