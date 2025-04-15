{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game.DealerTurn (decideDealerPlay, evolveDealerTurn) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Domain
import GameTopology

decideDealerPlay :: Game phase -> Decision
decideDealerPlay = \case
  Game {state = DealerTurnState InsuranceContext {context = GameContext {deck, dealer}}} ->
    let dealer' = dealerTurn dealer deck
     in Right (DealerPlayed dealer')
  Game {state = PlayerTurnState {}} -> Left PlayersStillPlaying
  _ -> Left BadCommand
  where
    dealerTurn :: Dealer -> Deck -> Dealer
    dealerTurn dealer@(Dealer hand) deck
      | not (dealerShouldHit dealer) = dealer
      | otherwise = case drawCard deck of
          Nothing -> dealer
          Just (card, newDeck) -> dealerTurn (Dealer (addCard card hand)) newDeck

evolveDealerTurn :: Game DealerTurn -> Event -> EvolutionResult GameTopology Game DealerTurn output
evolveDealerTurn game@Game {state = DealerTurnState InsuranceContext {context = GameContext {rounds}, insurancePayouts}} = \case
  DealerPlayed dealer -> EvolutionResult game {state = ResolvingState (ResolutionContext rounds dealer insurancePayouts)}
  _ -> EvolutionResult game
