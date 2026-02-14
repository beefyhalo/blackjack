{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game.Result (decideResult, evolveResult) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Types
import GameTopology

decideResult :: Game phase -> ResultCommand -> Either GameError ResultEvent
decideResult Game {state = ResultState {}} RestartGame = Right GameRestarted
decideResult _ _ = Left GameAlreadyStarted

evolveResult :: Game Result -> ResultEvent -> EvolutionResult GameTopology Game Result output
evolveResult game@Game {state = ResultState players} = \case
  GameRestarted -> EvolutionResult $ withUpdatedRng game {state = LobbyState players}
  GameExited -> EvolutionResult game {state = ExitedState}
