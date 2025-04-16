{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game.DealerTurn (decideDealerPlay, evolveDealerTurn) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Domain
import GameTopology

decideDealerPlay :: Game phase -> DealerTurnCommand -> Either GameError DealerTurnEvent
decideDealerPlay Game {state = DealerTurnState InsuranceContext {context = GameContext {deck, dealer}}} = \case
  DealerPlay ->
    let dealer' = dealerPlay dealer deck
     in Right (DealerPlayed dealer')
  where
    dealerPlay :: Dealer -> Deck -> Dealer
    dealerPlay dealer0@(Dealer hand) deck0
      | not (dealerShouldHit dealer0) = dealer0
      | otherwise = case drawCard deck0 of
          Nothing -> dealer0
          Just (card, newDeck) -> dealerPlay (Dealer (addCard card hand)) newDeck
decideDealerPlay Game {state = PlayerTurnState {}} = \_ -> Left PlayersStillPlaying
decideDealerPlay _ = \_ -> Left BadCommand

evolveDealerTurn :: Game DealerTurn -> DealerTurnEvent -> EvolutionResult GameTopology Game DealerTurn output
evolveDealerTurn game@Game {state = DealerTurnState InsuranceContext {context = GameContext {rounds}, insurancePayouts}} = \case
  DealerPlayed dealer -> EvolutionResult game {state = ResolvingState (ResolutionContext rounds dealer insurancePayouts)}
