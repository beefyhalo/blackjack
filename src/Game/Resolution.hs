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

decideResolveRound :: Game vertex -> Decision
decideResolveRound = \case
  Game {state = ResolvingState ResolutionContext {resolvedRounds, resolvedDealer, resolvedInsurancePayouts}} ->
    let dealerOutcome = determineDealerOutcome resolvedDealer
        playerSummaries = Map.mapWithKey (resolvePlayer dealerOutcome resolvedInsurancePayouts) resolvedRounds
     in Right (RoundResolved dealerOutcome playerSummaries)
  _ -> Left BadCommand
  where
    determineDealerOutcome :: Dealer -> DealerOutcome
    determineDealerOutcome (Dealer hand)
      | isBlackjack hand = DealerBlackjack
      | isBust hand = DealerBust
      | otherwise = DealerFinalScore (score hand)

    resolvePlayer :: DealerOutcome -> Map.Map PlayerId InsurancePayout -> PlayerId -> PlayerSession -> PlayerSummary
    resolvePlayer dealerOutcome insurancePayouts pid PlayerSession {hands, player = Player {stack}, hasSurrendered} =
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
          let outcome = determineOutcome hand dealerOutcome
              delta = chipsDelta bet outcome
              pushAmount = if outcome == Push then bet else 0
           in (outcome, delta, pushAmount)

        determineOutcome :: HandState -> DealerOutcome -> Outcome
        determineOutcome HandState {hand} = \case
          DealerBlackjack
            | isBlackjack hand -> Push
            | otherwise -> DealerWins OutscoredByDealer
          DealerBust
            | hasSurrendered -> DealerWins Surrendered -- Surrender always results in dealer win
            | isBust hand -> DealerWins PlayerBust -- Both busting = dealer wins
            | otherwise -> PlayerWins OutscoredDealer
          DealerFinalScore dealerScore
            | hasSurrendered -> DealerWins Surrendered -- Surrender always results in dealer win
            | isBlackjack hand -> PlayerWins Blackjack
            | isBust hand -> DealerWins PlayerBust
            | otherwise -> case compare (score hand) dealerScore of
                GT -> PlayerWins OutscoredDealer
                LT -> DealerWins OutscoredByDealer
                EQ -> Push

        chipsDelta :: Bet -> Outcome -> Chips
        chipsDelta Bet {current} = \case
          PlayerWins Blackjack -> payout 1.5
          PlayerWins _ -> payout 1.0
          DealerWins Surrendered -> -(current `div` 2) -- Player loses half their bet when surrendering
          DealerWins _ -> -current
          Push -> 0
          where
            payout m = floor @Float (fromIntegral current * m)

evolveResolution :: Game ResolvingHands -> Event -> EvolutionResult GameTopology Game ResolvingHands output
evolveResolution game@Game {state = ResolvingState ResolutionContext {resolvedRounds}} = \case
  RoundResolved _ outcomes ->
    let settle PlayerSession {player} (PlayerSummary {nextRoundBet, finalChips}) =
          player {stack = PlayerStack nextRoundBet finalChips}
        players = Map.mapWithKey (settle . (resolvedRounds Map.!)) outcomes
     in EvolutionResult game {state = ResultState players}
  _ -> EvolutionResult game
