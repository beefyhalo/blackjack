{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game.Lobby
  ( decideJoinGame,
    decideLeaveGame,
    decideStartGame,
    evolveLobby,
  )
where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Domain
import GameTopology

decideJoinGame :: Text -> Game vertex -> Decision
decideJoinGame name = \case
  Game {state = LobbyState players} ->
    let pid = PlayerId (length players)
     in Right (PlayerJoined pid name)
  _ -> Left GameAlreadyStarted

decideLeaveGame :: PlayerId -> Game vertex -> Decision
decideLeaveGame pid = \case
  Game {state = LobbyState players}
    | Map.member pid players -> Right (PlayerLeft pid)
    | otherwise -> Left PlayerNotFound
  _ -> Left GameAlreadyStarted

decideStartGame :: Game vertex -> Decision
decideStartGame = \case
  Game {state = LobbyState players}
    | null players -> Left TooFewPlayers
    | otherwise -> Right GameStarted
  _ -> Left GameAlreadyStarted

evolveLobby :: Game InLobby -> Event -> EvolutionResult GameTopology Game InLobby output
evolveLobby game@Game {state = LobbyState players} = \case
  PlayerJoined pid name ->
    let players' = Map.insert pid (newPlayer pid name) players
     in EvolutionResult game {state = LobbyState players'}
  PlayerLeft pid ->
    let players' = Map.delete pid players
     in EvolutionResult game {state = LobbyState players'}
  GameStarted -> EvolutionResult game {state = BettingState players}
  _ -> EvolutionResult game
