{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game.Resolution (decideResolution, evolveResolution) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Foldable (toList)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Domain
import GameTopology
import Prelude hiding (round)

decideResolution :: Game phase -> ResolutionCommand -> Either GameError ResolutionEvent
decideResolution Game {state = ResolvingState ResolutionContext {resolvedRounds, resolvedDealer, resolvedInsurancePayouts}} = \case
  ResolveRound ->
    let dealerOutcome = determineDealerOutcome resolvedDealer
        playerSummaries = Map.mapWithKey (resolvePlayer dealerOutcome) resolvedRounds
     in Right (RoundResolved dealerOutcome playerSummaries)
  where
    resolvePlayer :: DealerOutcome -> PlayerId -> PlayerRound -> PlayerSummary
    resolvePlayer dealerOutcome pid round@PlayerRound {hands, player = Player {stack}} =
      let (outcomes, totalDelta, totalPush) = unzip3 $ map resolveHand (toList hands)
          net = sum totalDelta
          pushed = sum totalPush
          insurancePayout = Map.lookup pid resolvedInsurancePayouts
          insuranceNet = maybe 0 insuranceDelta insurancePayout
       in PlayerSummary
            { handOutcomes = NE.fromList outcomes,
              netChipChange = net + insuranceNet,
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
decideResolution _ = \_ -> Left BadCommand

evolveResolution :: Game ResolvingHands -> ResolutionEvent -> EvolutionResult GameTopology Game ResolvingHands output
evolveResolution game@Game {state = ResolvingState ResolutionContext {resolvedRounds}} = \case
  RoundResolved _ outcomes ->
    let settle PlayerRound {player} (PlayerSummary {nextRoundBet, finalChips}) =
          player {stack = PlayerStack nextRoundBet finalChips}
        players = Map.mapWithKey (settle . (resolvedRounds Map.!)) outcomes
     in EvolutionResult game {state = ResultState players}
