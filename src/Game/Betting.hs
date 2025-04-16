{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game.Betting (decideBetting, evolveBetting) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Map.Strict qualified as Map
import Domain
import GameTopology

decideBetting :: Game phase -> BettingCommand -> Either GameError BettingEvent
decideBetting = \case
  Game {state = BettingState players} -> \case
    PlaceBet pid bet ->
      case Map.lookup pid players of
        Nothing -> Left (PlayerNotFound pid)
        Just Player {stack = PlayerStack currentBet chips}
          | currentBet > 0 -> Left (PlayerAlreadyBet pid)
          | otherwise -> withValidBet bet chips (Right . BetPlaced pid)
  _ -> \_ -> Left BadCommand

evolveBetting :: Game AwaitingBets -> BettingEvent -> EvolutionResult GameTopology Game AwaitingBets output
evolveBetting game@Game {stdGen, state = BettingState players} = \case
  BetPlaced pid bet ->
    let updateBet player = player {stack = (stack player) {currentBet = bet}}
        players' = Map.adjust updateBet pid players
        allBetsIn = all ((> 0) . currentBet . stack) players'
     in if allBetsIn
          then EvolutionResult game {state = DealingState players' (mkDeck stdGen)}
          else EvolutionResult game {state = BettingState players'}
