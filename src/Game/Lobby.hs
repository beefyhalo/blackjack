{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game.Lobby (decideLobby, evolveLobby) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Map.Strict qualified as Map
import GameTopology
import Types

decideLobby :: Game phase -> LobbyCommand -> Either GameError LobbyEvent
decideLobby Game {state = LobbyState players, nextPlayerId} = \case
  JoinGame name -> Right (PlayerJoined (PlayerId nextPlayerId) name)
  LeaveGame pid
    | Map.notMember pid players -> Left (PlayerNotFound pid)
    | otherwise -> Right (PlayerLeft pid)
  StartGame
    | null players -> Left TooFewPlayers
    | otherwise -> Right GameStarted
decideLobby _ = const (Left GameAlreadyStarted)

evolveLobby :: Game InLobby -> LobbyEvent -> EvolutionResult GameTopology Game InLobby output
evolveLobby game@Game {state = LobbyState players, nextPlayerId} = \case
  PlayerJoined pid name -> EvolutionResult game {state = LobbyState players', nextPlayerId = nextPlayerId + 1}
    where players' = Map.insert pid (newPlayer pid name) players
  PlayerLeft pid -> EvolutionResult game {state = LobbyState players'}
    where players' = Map.delete pid players
  GameStarted -> EvolutionResult game {state = BettingState players}
