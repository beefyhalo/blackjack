{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game.Result where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Domain
import GameTopology

decideRestartGame :: Game vertex -> Decision
decideRestartGame = \case
  Game {state = ResultState {}} -> Right GameRestarted
  _ -> Left GameAlreadyStarted

evolveResult :: Game Result -> Event -> EvolutionResult GameTopology Game Result output
evolveResult game@Game {state = ResultState seats} = \case
  GameRestarted -> EvolutionResult (withUpdatedRng game {state = LobbyState seats})
  GameExited -> EvolutionResult game {state = ExitedState}
  _ -> EvolutionResult game
