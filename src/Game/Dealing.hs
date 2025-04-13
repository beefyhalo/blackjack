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

decideDealInitialCards :: Game vertex -> Decision
decideDealInitialCards = \case
  Game {state = DealingState pids deck} ->
    maybe (Left EmptyDeck) Right do
      (playerHands, deck') <- dealNTo 2 (Map.keys pids) deck
      (dealerHand, _) <- dealN 2 deck'
      Just (CardsDealt playerHands (Dealer dealerHand))
  Game {state = BiddingState {}} -> Left PlayersStillBetting
  _ -> Left BadCommand

evolveDealing :: Game DealingCards -> Event -> EvolutionResult GameTopology Game DealingCards output
evolveDealing game@Game {state = DealingState seats deck} = \case
  CardsDealt playerHands dealer@(Dealer dealerHand)
    | isAce (visibleCard dealer) ->
        let players = fmap (initPlayer emptyHand) seats
         in EvolutionResult game {state = OfferingInsuranceState deck' players dealer}
    | otherwise ->
        let players = Map.fromList [(pid, initPlayer hand (seats Map.! pid)) | (pid, hand) <- playerHands]
         in EvolutionResult game {state = OpeningTurnState deck' players dealer Map.empty Set.empty}
    where
      deck' =
        let cardsDrawn = sum (map (handSize . snd) playerHands) + handSize dealerHand
         in deck {drawn = drawn deck + cardsDrawn}
  _ -> EvolutionResult game
