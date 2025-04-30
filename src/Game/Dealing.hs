{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Game.Dealing (decideDealing, evolveDealing) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GameTopology
import Types

decideDealing :: Game phase -> DealingCommand -> Either GameError DealingEvent
decideDealing Game {state = DealingState pids deck} = \case
  DealInitialCards ->
    maybe (Left EmptyDeck) Right do
      (playerHands, deck') <- dealNTo 2 (Map.keys pids) deck
      (dealerHand, _) <- dealN 2 deck'
      Just (CardsDealt playerHands (Dealer dealerHand))
decideDealing Game {state = BettingState {}} = \_ -> Left PlayersStillBetting
decideDealing _ = \_ -> Left BadCommand

evolveDealing :: Game DealingCards -> DealingEvent -> EvolutionResult GameTopology Game DealingCards output
evolveDealing game@Game {state = DealingState players deck} = \case
  CardsDealt playerHands dealer
    | isAce (visibleCard dealer) ->
        EvolutionResult game {state = OfferingInsuranceState context}
    | otherwise ->
        let openingContext = OpeningContext (InsuranceContext context Map.empty) Set.empty
         in EvolutionResult game {state = OpeningTurnState openingContext}
    where
      context =
        let rounds =
              Map.fromList
                [(pid, initPlayerRound hand (players Map.! pid)) | (pid, hand) <- playerHands]
            deck' =
              let cardsDrawn = sum [handSize hand | (_, hand) <- playerHands] + handSize dealer.dealerHand
               in deck {drawn = drawn deck + cardsDrawn}
         in GameContext deck' rounds dealer
