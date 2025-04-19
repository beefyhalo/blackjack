{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Game.DealerTurn (decideDealerPlay, evolveDealerTurn) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Domain
import GameTopology

decideDealerPlay :: Game phase -> DealerTurnCommand -> Either GameError DealerTurnEvent
decideDealerPlay Game {state = DealerTurnState InsuranceContext {context}} = \case
  DealerPlay ->
    let dealer' = dealerPlay context.dealer context.deck
     in Right (DealerPlayed dealer')
  where
    dealerPlay :: Dealer -> Deck -> Dealer
    dealerPlay dealer@(Dealer hand) deck
      | not (dealerShouldHit dealer) = dealer
      | otherwise = case drawCard deck of
          Nothing -> dealer
          Just (card, newDeck) -> dealerPlay (Dealer (addCard card hand)) newDeck
decideDealerPlay Game {state = PlayerTurnState {}} = \_ -> Left PlayersStillPlaying
decideDealerPlay _ = \_ -> Left BadCommand

evolveDealerTurn :: Game DealerTurn -> DealerTurnEvent -> EvolutionResult GameTopology Game DealerTurn output
evolveDealerTurn game@Game {state = DealerTurnState InsuranceContext {context = GameContext {rounds}, insurancePayouts}} = \case
  DealerPlayed dealer -> EvolutionResult game {state = ResolvingState (ResolutionContext rounds dealer insurancePayouts)}
