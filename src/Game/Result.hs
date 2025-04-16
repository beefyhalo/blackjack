{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game.Result (decideResult, evolveResult) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Domain
import GameTopology

decideResult :: Game phase -> ResultCommand -> Decision
decideResult = \case
  Game {state = ResultState {}} -> \case
    RestartGame -> Right (ResultEvt GameRestarted)
  _ -> \_ -> Left GameAlreadyStarted

evolveResult :: Game Result -> ResultEvent -> EvolutionResult GameTopology Game Result output
evolveResult game@Game {state = ResultState players} = \case
  GameRestarted -> EvolutionResult (withUpdatedRng game {state = LobbyState players})
  GameExited -> EvolutionResult game {state = ExitedState}
