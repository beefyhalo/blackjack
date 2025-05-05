{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Game.Resolution (decideResolution, evolveResolution) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Foldable (toList)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import GameTopology
import Types
import Prelude hiding (round)

decideResolution :: Game phase -> ResolutionCommand -> Either GameError ResolutionEvent
decideResolution Game {state = ResolvingState ctx} = \case
  ResolveRound ->
    let dealerOutcome = determineDealerOutcome ctx.resolvedDealer
        playerSummaries = Map.mapWithKey (resolvePlayer dealerOutcome) ctx.resolvedRounds
     in Right (RoundResolved dealerOutcome playerSummaries)
  where
    resolvePlayer :: DealerOutcome -> PlayerId -> PlayerRound -> PlayerSummary
    resolvePlayer dealerOutcome pid round =
      let (outcomes, totalDelta, totalPush) = unzip3 $ map resolveHand (toList round.hands)
          net = sum totalDelta
          pushed = sum totalPush
          insurancePayout = Map.lookup pid ctx.resolvedInsurancePayouts
          insuranceNet = maybe 0 insuranceDelta insurancePayout
       in PlayerSummary
            { handOutcomes = NE.fromList outcomes,
              netChipChange = net + insuranceNet,
              finalChips = round.player.stack.chips + net,
              nextRoundBet = pushed,
              insurancePayout = insurancePayout
            }
      where
        resolveHand :: HandState -> (Outcome, Int, Bet)
        resolveHand hand =
          let outcome = determinePlayerOutcome round hand dealerOutcome
              delta = chipsDelta hand.bet outcome
              pushAmount = if outcome == Push then hand.bet else 0
           in (outcome, delta, pushAmount)
decideResolution _ = \_ -> Left BadCommand

evolveResolution :: Game ResolvingHands -> ResolutionEvent -> EvolutionResult GameTopology Game ResolvingHands output
evolveResolution game@Game {state = ResolvingState ctx} = \case
  RoundResolved _ outcomes ->
    let settle round PlayerSummary {nextRoundBet, finalChips} =
          round.player {stack = PlayerStack nextRoundBet finalChips}
        players = Map.mapWithKey (settle . (ctx.resolvedRounds Map.!)) outcomes
     in EvolutionResult game {state = ResultState players}
