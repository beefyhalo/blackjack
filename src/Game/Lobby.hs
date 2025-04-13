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
import Domain (Event (GameStarted, PlayerJoined, PlayerLeft), GameError (GameAlreadyStarted, PlayerNotFound, TooFewPlayers), PlayerId (..), newPlayerSeat)
import GameTopology (Decision, Game (Game, state), GameState (BiddingState, LobbyState), GameTopology, GameVertex (InLobby))

decideJoinGame :: Text -> Game vertex -> Decision
decideJoinGame name = \case
  Game {state = LobbyState seats} ->
    let pid = PlayerId (length seats)
     in Right (PlayerJoined pid name)
  _ -> Left GameAlreadyStarted

decideLeaveGame :: PlayerId -> Game vertex -> Decision
decideLeaveGame pid = \case
  Game {state = LobbyState seats}
    | Map.member pid seats -> Right (PlayerLeft pid)
    | otherwise -> Left PlayerNotFound
  _ -> Left GameAlreadyStarted

decideStartGame :: Game vertex -> Decision
decideStartGame = \case
  Game {state = LobbyState seats}
    | null seats -> Left TooFewPlayers
    | otherwise -> Right GameStarted
  _ -> Left GameAlreadyStarted

evolveLobby :: Game InLobby -> Event -> EvolutionResult GameTopology Game InLobby output
evolveLobby game@Game {state = LobbyState seats} = \case
  PlayerJoined pid name ->
    let seats' = Map.insert pid (newPlayerSeat pid name) seats
     in EvolutionResult game {state = LobbyState seats'}
  PlayerLeft pid ->
    let seats' = Map.delete pid seats
     in EvolutionResult game {state = LobbyState seats'}
  GameStarted -> EvolutionResult game {state = BiddingState seats}
  _ -> EvolutionResult game
