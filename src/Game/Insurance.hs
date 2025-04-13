{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game.Insurance where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Domain
import GameTopology

decideTakeInsurance :: PlayerId -> Chips -> Game vertex -> Decision
decideTakeInsurance pid insuranceChips = \case
  Game {state = OfferingInsuranceState _ players _}
    | not (Map.member pid players) -> Left PlayerNotFound
    | isJust (insurance (players Map.! pid)) -> Left PlayerAlreadyInsured
    | let Player {playerSeat = PlayerSeat {stack}} = players Map.! pid,
      0 > insuranceChips || insuranceChips < chips stack ->
        Left MalsizedBet
    | otherwise -> Right (PlayerTookInsurance pid insuranceChips)
  _ -> Left BadCommand

decideRejectInsurance :: PlayerId -> Game vertex -> Decision
decideRejectInsurance pid = \case
  Game {state = OfferingInsuranceState _ players _}
    | not (Map.member pid players) -> Left PlayerNotFound
    | isJust (insurance (players Map.! pid)) -> Left PlayerAlreadyInsured
    | otherwise -> Right (PlayerDeclinedInsurance pid)
  _ -> Left BadCommand

decideResolveInsurance :: Game vertex -> Decision
decideResolveInsurance = \case
  Game {state = ResolvingInsuranceState _ players (Dealer dealerHand)} ->
    let isDealerBlackjack = isBlackjack dealerHand
        results = fmap (resolveInsuranceForPlayer isDealerBlackjack) players
     in Right (InsuranceResolved results)
  _ -> Left BadCommand
  where
    resolveInsuranceForPlayer :: Bool -> Player -> InsuranceResult
    resolveInsuranceForPlayer hasBJ Player {insurance} = case insurance of
      Just (TookInsurance amt)
        | hasBJ -> WonInsurancePayout (amt * 2) -- 2:1 insurance payout if the dealer has a blackjack
        | otherwise -> LostInsuranceBet amt
      _ -> NoInsurance

evolveOfferingInsurance :: Game OfferingInsurance -> Event -> EvolutionResult GameTopology Game OfferingInsurance output
evolveOfferingInsurance game@Game {state = OfferingInsuranceState deck players dealer} = \case
  PlayerTookInsurance pid chips ->
    let players' = Map.adjust (\p -> p {insurance = Just (TookInsurance chips)}) pid players
     in nextState players'
  PlayerDeclinedInsurance pid ->
    let players' = Map.adjust (\p -> p {insurance = Just DeclinedInsurance}) pid players
     in nextState players'
  _ -> EvolutionResult game
  where
    nextState players'
      | all (isJust . insurance) players' = EvolutionResult game {state = ResolvingInsuranceState deck players' dealer}
      | otherwise = EvolutionResult game {state = OfferingInsuranceState deck players' dealer}

evolveResolvingInsurance :: Game ResolvingInsurance -> Event -> EvolutionResult GameTopology Game ResolvingInsurance output
evolveResolvingInsurance game@Game {state = ResolvingInsuranceState deck players dealer@(Dealer dealerHand)} = \case
  InsuranceResolved results
    | isBlackjack dealerHand ->
        let players' = Map.mapWithKey settleInsurance results
         in if isBlackjack dealerHand
              then EvolutionResult game {state = ResolvingState players' dealer}
              else EvolutionResult game {state = OpeningTurnState deck players' dealer Set.empty}
  _ -> EvolutionResult game
  where
    settleInsurance :: PlayerId -> InsuranceResult -> Player
    settleInsurance pid result =
      let player = players Map.! pid
          PlayerSeat {stack} = playerSeat player
          delta = case result of
            WonInsurancePayout amt -> amt
            LostInsuranceBet amt -> -amt
            NoInsurance -> 0
          playerSeat' = (playerSeat player) {stack = stack {chips = chips stack + delta}}
       in player {playerSeat = playerSeat'}
