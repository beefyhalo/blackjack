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
  ResolveRound -> Right (RoundResolved dealerOutcome playerSummaries)
    where
      dealerOutcome = determineDealerOutcome ctx.resolvedDealer
      playerSummaries = Map.mapWithKey (resolvePlayer dealerOutcome) ctx.resolvedRounds

      resolvePlayer :: DealerOutcome -> PlayerId -> PlayerRound -> PlayerSummary
      resolvePlayer dealerOutcome' pid round =
        PlayerSummary
          { handOutcomes = NE.fromList outcomes,
            netChipChange = net + insuranceNet,
            finalChips = round.player.stack.chips + net,
            nextRoundBet,
            insurancePayout
          }
        where
          (outcomes, totalDelta, totalPush) = unzip3 $ map resolveHand (toList round.hands)
          net = sum totalDelta
          nextRoundBet = sum totalPush
          insurancePayout = Map.lookup pid ctx.resolvedInsurancePayouts
          insuranceNet = maybe 0 insuranceDelta insurancePayout

          resolveHand :: HandState -> (Outcome, Int, Bet)
          resolveHand hand = (outcome, delta, pushAmount)
            where
              outcome = determinePlayerOutcome round hand dealerOutcome'
              delta = chipsDelta hand.bet outcome
              pushAmount = if outcome == Push then hand.bet else 0
decideResolution _ = \_ -> Left BadCommand

evolveResolution :: Game ResolvingHands -> ResolutionEvent -> EvolutionResult GameTopology Game ResolvingHands output
evolveResolution game@Game {state = ResolvingState ctx} = \case
  RoundResolved _ outcomes -> EvolutionResult game {state = ResultState players}
    where
      players = Map.mapWithKey (settle . (ctx.resolvedRounds Map.!)) outcomes
      settle round PlayerSummary {nextRoundBet, finalChips} =
        round.player {stack = PlayerStack nextRoundBet finalChips}
