{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Test.Lobby (tests) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Map.Strict qualified as Map
import Types
import Game.Gen
import Game.Lobby (decideLobby, evolveLobby)
import GameTopology (SomeGame(SomeGame), Game (Game, state), GameState (BettingState, LobbyState))
import Hedgehog
import Hedgehog.Gen qualified as Gen

tests :: IO Bool
tests = checkParallel $$discover

-- decide emits a PlayerJoined event when joining a lobby
prop_decide_join_emits_PlayerJoined :: Property
prop_decide_join_emits_PlayerJoined = property do
  name <- forAll genPlayerName
  game <- forAll genLobbyStateGame
  case decideLobby game (JoinGame name) of
    Right (PlayerJoined _ name') -> name === name'
    _ -> failure

-- decide rejects if not in the lobby
prop_decide_rejects_JoinGame_in_non_lobby_state :: Property
prop_decide_rejects_JoinGame_in_non_lobby_state = property do
  SomeGame game <- forAllNonLobbyStateGame
  name <- forAll genPlayerName
  decideLobby game (JoinGame name) === Left GameAlreadyStarted

-- evolve updates the state with a joined player
prop_evolve_PlayerJoined_adds_player :: Property
prop_evolve_PlayerJoined_adds_player = property do
  pid <- forAll genPlayerId
  name <- forAll genPlayerName
  game <- forAll genLobbyStateGame
  let evolved = evolveLobby game (PlayerJoined pid name)
  assert case evolved of
    EvolutionResult Game {state = LobbyState players} -> Map.member pid players
    _ -> False

-- decide emits PlayerLeft if the player is in the lobby
prop_decide_leave_emits_PlayerLeft :: Property
prop_decide_leave_emits_PlayerLeft = property do
  pid <- forAll genPlayerId
  player <- forAll genPlayer
  game@Game {state = LobbyState players} <- forAll genLobbyStateGame
  let game' = game {state = LobbyState (Map.insert pid player players)}
  decideLobby game' (LeaveGame pid) === Right (PlayerLeft pid)

-- decide rejects if not in the lobby
prop_decide_rejects_LeaveGame_in_non_lobby_state :: Property
prop_decide_rejects_LeaveGame_in_non_lobby_state = property do
  SomeGame game <- forAllNonLobbyStateGame
  pid <- forAll genPlayerId
  decideLobby game (LeaveGame pid) === Left GameAlreadyStarted

-- evolve removes a player with player left
prop_evolve_PlayerLeft_removes_player :: Property
prop_evolve_PlayerLeft_removes_player = property do
  pid <- forAll genPlayerId
  game <- forAll genLobbyStateGame
  let evolved = evolveLobby game (PlayerLeft pid)
  case evolved of
    EvolutionResult Game {state = LobbyState players} -> assert (Map.notMember pid players)
    _ -> failure

--  decide emits a GameStarted when starting with a joined player in lobby
prop_decide_start_emits_GameStarted :: Property
prop_decide_start_emits_GameStarted = property do
  game <- forAll $ Gen.filter (\Game {state = LobbyState players} -> not (null players)) genLobbyStateGame
  decideLobby game StartGame === Right GameStarted

-- decide rejects StartGame if not in the lobby
prop_decide_rejects_StartGame_in_non_lobby_state :: Property
prop_decide_rejects_StartGame_in_non_lobby_state = property do
  SomeGame game <- forAllNonLobbyStateGame
  decideLobby game StartGame === Left GameAlreadyStarted

-- decide rejects starting with an empty lobby
prop_decide_start_rejects_empty_lobby :: Property
prop_decide_start_rejects_empty_lobby = property do
  game <- forAll genLobbyStateGame
  let game' = game {state = LobbyState Map.empty}
  decideLobby game' StartGame === Left TooFewPlayers

-- evolve GameStarted advances the game state
prop_evolve_GameStarted_advances_state :: Property
prop_evolve_GameStarted_advances_state = property do
  game@Game {state = LobbyState players} <- forAll genLobbyStateGame
  let evolved = evolveLobby game GameStarted
  case evolved of
    EvolutionResult Game {state = BettingState players'} -> players === players'
    _ -> failure

forAllNonLobbyStateGame :: PropertyT IO SomeGame
forAllNonLobbyStateGame = do
  forAllWith (\(SomeGame g) -> show g) $
    Gen.filter (\case SomeGame Game {state = LobbyState {}} -> False; _ -> True) genGame
