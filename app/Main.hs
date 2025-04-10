{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Blackjack
import Card
import Data.List.NonEmpty qualified as NE

main :: IO ()
main = do
  let initialDeck = fullDeck
  let initialPlayers = [Player {hand = emptyHand, bet = Bet 0 100}]
  let game = Game initialDeck (NE.fromList initialPlayers) emptyHand
  gameLoop (SomeGameState (StartState game))