{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game.DealerTurn (decideDealerPlay, evolveDealerTurn) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Domain
import GameTopology

decideDealerPlay :: Game vertex -> Decision
decideDealerPlay = \case
  Game {state = DealerTurnState InsuranceContext {context = GameContext {deck, dealer}}} ->
    let dealer' = dealerTurn dealer deck
     in Right (DealerPlayed dealer')
  Game {state = PlayerTurnState {}} -> Left PlayersStillPlaying
  _ -> Left BadCommand
  where
    -- Draw cards until the hand has at least 17 points
    dealerTurn :: Dealer -> Deck -> Dealer
    dealerTurn dealer@(Dealer hand) deck
      | score hand >= 17 = dealer
      | otherwise = case drawCard deck of
          Nothing -> dealer
          Just (card, newDeck) -> dealerTurn (Dealer (addCard card hand)) newDeck

evolveDealerTurn :: Game DealerTurn -> Event -> EvolutionResult GameTopology Game DealerTurn output
evolveDealerTurn game@Game {state = DealerTurnState InsuranceContext {context = GameContext {players}, insurancePayouts}} = \case
  DealerPlayed dealer -> EvolutionResult game {state = ResolvingState (ResolutionContext players dealer insurancePayouts)}
  _ -> EvolutionResult game
