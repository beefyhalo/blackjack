{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Game.Betting (decideBetting, evolveBetting) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Map.Strict qualified as Map
import GameTopology
import Types

decideBetting :: Game phase -> BettingCommand -> Either GameError BettingEvent
decideBetting Game {state = BettingState players} = \case
  PlaceBet pid bet ->
    case Map.lookup pid players of
      Nothing -> Left (PlayerNotFound pid)
      Just player
        | player.stack.currentBet > 0 -> Left (PlayerAlreadyBet pid)
        | otherwise -> withValidBet bet player.stack.chips (Right . BetPlaced pid)
decideBetting _ = \_ -> Left BadCommand

evolveBetting :: Game AwaitingBets -> BettingEvent -> EvolutionResult GameTopology Game AwaitingBets output
evolveBetting game@Game {stdGen, state = BettingState players} = \case
  BetPlaced pid bet
    | allBetsIn -> EvolutionResult game {state = DealingState players' (mkDeck stdGen)}
    | otherwise -> EvolutionResult game {state = BettingState players'}
    where
      updateBet player = player {stack = player.stack {currentBet = bet}}
      players' = Map.adjust updateBet pid players
      allBetsIn = all (\p -> p.stack.currentBet > 0) players'
