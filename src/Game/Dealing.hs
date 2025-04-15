{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game.Dealing (decideDealInitialCards, evolveDealing) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Domain
import GameTopology

decideDealInitialCards :: Game phase -> Decision
decideDealInitialCards = \case
  Game {state = DealingState pids deck} ->
    maybe (Left EmptyDeck) Right do
      (playerHands, deck') <- dealNTo 2 (Map.keys pids) deck
      (dealerHand, _) <- dealN 2 deck'
      Just (CardsDealt playerHands (Dealer dealerHand))
  Game {state = BettingState {}} -> Left PlayersStillBetting
  _ -> Left BadCommand

evolveDealing :: Game DealingCards -> Event -> EvolutionResult GameTopology Game DealingCards output
evolveDealing game@Game {state = DealingState players deck} = \case
  CardsDealt playerHands dealer@(Dealer dealerHand)
    | isAce (visibleCard dealer) ->
        let rounds = fmap (initPlayerRound emptyHand) players
         in EvolutionResult game {state = OfferingInsuranceState (GameContext deck' rounds dealer)}
    | otherwise ->
        let rounds = Map.fromList [(pid, initPlayerRound hand (players Map.! pid)) | (pid, hand) <- playerHands]
            openingContext = OpeningContext (InsuranceContext (GameContext deck' rounds dealer) Map.empty) Set.empty
         in EvolutionResult game {state = OpeningTurnState openingContext}
    where
      deck' =
        let cardsDrawn = sum (map (handSize . snd) playerHands) + handSize dealerHand
         in deck {drawn = drawn deck + cardsDrawn}
  _ -> EvolutionResult game
