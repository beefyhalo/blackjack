{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game.DealerTurn (decideDealerPlay, evolveDealerTurn) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Domain
import GameTopology

decideDealerPlay :: Game phase -> DealerTurnCommand -> Decision
decideDealerPlay = \case
  Game {state = DealerTurnState InsuranceContext {context = GameContext {deck, dealer}}} -> \case
    DealerPlay ->
      let dealer' = dealerPlay dealer deck
       in Right (DealerTurnEvt $ DealerPlayed dealer')
  Game {state = PlayerTurnState {}} -> \_ -> Left PlayersStillPlaying
  _ -> \_ -> Left BadCommand
  where
    dealerPlay :: Dealer -> Deck -> Dealer
    dealerPlay dealer@(Dealer hand) deck
      | not (dealerShouldHit dealer) = dealer
      | otherwise = case drawCard deck of
          Nothing -> dealer
          Just (card, newDeck) -> dealerPlay (Dealer (addCard card hand)) newDeck

evolveDealerTurn :: Game DealerTurn -> DealerTurnEvent -> EvolutionResult GameTopology Game DealerTurn output
evolveDealerTurn game@Game {state = DealerTurnState InsuranceContext {context = GameContext {rounds}, insurancePayouts}} = \case
  DealerPlayed dealer -> EvolutionResult game {state = ResolvingState (ResolutionContext rounds dealer insurancePayouts)}
