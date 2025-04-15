{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game.Resolution (decideResolveRound, evolveResolution) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Foldable (toList)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Domain
import GameTopology
import Prelude hiding (round)

decideResolveRound :: Game phase -> Decision
decideResolveRound = \case
  Game {state = ResolvingState ResolutionContext {resolvedRounds, resolvedDealer, resolvedInsurancePayouts}} ->
    let dealerOutcome = determineDealerOutcome resolvedDealer
        playerSummaries = Map.mapWithKey (resolvePlayer dealerOutcome resolvedInsurancePayouts) resolvedRounds
     in Right (RoundResolved dealerOutcome playerSummaries)
  _ -> Left BadCommand
  where
    resolvePlayer :: DealerOutcome -> Map.Map PlayerId InsurancePayout -> PlayerId -> PlayerRound -> PlayerSummary
    resolvePlayer dealerOutcome insurancePayouts pid round@PlayerRound {hands, player = Player {stack}} =
      let (outcomes, totalDelta, totalPush) = unzip3 $ map resolveHand (toList hands)
          net = sum totalDelta
          pushed = sum totalPush
          insurancePayout = Map.lookup pid insurancePayouts
          insuranceDelta = case insurancePayout of
            Just (WonInsurancePayout amt) -> amt
            Just (LostInsuranceBet amt) -> -amt
            _ -> 0
       in PlayerSummary
            { handOutcomes = NE.fromList outcomes,
              netChipChange = net + insuranceDelta,
              finalChips = chips stack + net,
              nextRoundBet = pushed,
              insurancePayout = insurancePayout
            }
      where
        resolveHand :: HandState -> (Outcome, Int, Bet)
        resolveHand hand@HandState {bet} =
          let outcome = determineOutcome round hand dealerOutcome
              delta = chipsDelta bet outcome
              pushAmount = if outcome == Push then bet else 0
           in (outcome, delta, pushAmount)

evolveResolution :: Game ResolvingHands -> Event -> EvolutionResult GameTopology Game ResolvingHands output
evolveResolution game@Game {state = ResolvingState ResolutionContext {resolvedRounds}} = \case
  RoundResolved _ outcomes ->
    let settle PlayerRound {player} (PlayerSummary {nextRoundBet, finalChips}) =
          player {stack = PlayerStack nextRoundBet finalChips}
        players = Map.mapWithKey (settle . (resolvedRounds Map.!)) outcomes
     in EvolutionResult game {state = ResultState players}
  _ -> EvolutionResult game
