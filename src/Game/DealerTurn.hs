{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Game.DealerTurn (decideDealerPlay, evolveDealerTurn) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import GameTopology
import Types

decideDealerPlay :: Game phase -> DealerTurnCommand -> Either GameError DealerTurnEvent
decideDealerPlay Game {state = DealerTurnState context} = \case
  DealerPlay ->
    let dealer = dealerPlay context.context.dealer context.context.deck
     in Right (DealerPlayed dealer)
  where
    dealerPlay :: Dealer -> Deck -> Dealer
    dealerPlay dealer deck
      | not (dealerShouldHit dealer) = dealer
      | Just (card, newDeck) <- drawCard deck = dealerPlay (Dealer (addCard card dealer.dealerHand)) newDeck
      | otherwise = dealer
decideDealerPlay Game {state = PlayerTurnState {}} = \_ -> Left PlayersStillPlaying
decideDealerPlay _ = \_ -> Left BadCommand

evolveDealerTurn :: Game DealerTurn -> DealerTurnEvent -> EvolutionResult GameTopology Game DealerTurn output
evolveDealerTurn game@Game {state = DealerTurnState ctx} = \case
  DealerPlayed dealer ->
    let context = ResolutionContext ctx.context.rounds dealer ctx.insurancePayouts
     in EvolutionResult game {state = ResolvingState context}
