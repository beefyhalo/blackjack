{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game.Result (decideRestartGame, evolveResult) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Domain
import GameTopology

decideRestartGame :: Game phase -> Decision
decideRestartGame = \case
  Game {state = ResultState {}} -> Right GameRestarted
  _ -> Left GameAlreadyStarted

evolveResult :: Game Result -> Event -> EvolutionResult GameTopology Game Result output
evolveResult game@Game {state = ResultState players} = \case
  GameRestarted -> EvolutionResult (withUpdatedRng game {state = LobbyState players})
  GameExited -> EvolutionResult game {state = ExitedState}
  _ -> EvolutionResult game
