{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Test.Lobby (tests) where

import Domain
import Game.Lobby (decideLobby)
import GameTopology (Game (Game), GameState (LobbyState))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Random (initStdGen)
import Game.Gen 

tests :: IO Bool
tests = checkSequential $$discover

-- decide emits a PlayerJoined event when joining a lobby
prop_decide_join_emits_PlayerJoined :: Property
prop_decide_join_emits_PlayerJoined = property do
  name <- forAll genPlayerName
  game <- forAll genLobbyStateGame
  case decideLobby game (JoinGame name) of
    Right (LobbyEvt (PlayerJoined _ name')) -> name === name'
    _ -> failure

-- decide rejects if not in the lobby

-- evolve updates the state with a joined player
-- prop_evolve_PlayerJoined_adds_player :: Property
-- prop_evolve_PlayerJoined_adds_player = property do
--   pid <- forAll genPlayerId
--   name <- undefined
--   let game = undefined $ LobbyState Map.empty
--       evolved = evolveLobby game (PlayerJoined pid name)
--   assert $ case evolved of
--     EvolutionResult Game {state = LobbyState players} -> Map.member pid players
--     _ -> False

-- -- decide emits PlayerLeft if the player is in the lobby
-- prop_decide_leave_emits_PlayerLeft :: Property
-- prop_decide_leave_emits_PlayerLeft = property do
--   pid <- forAll genPlayerId
--   let game = LobbyState [PlayerSeat pid 1000]
--   decide game (Leave pid) === Right [PlayerLeft pid]

-- -- decide rejects if not in the lobby

-- -- evolve removes a player with player left
-- prop_evolve_PlayerLeft_removes_player :: Property
-- prop_evolve_PlayerLeft_removes_player = property do
--   pid <- forAll genPlayerId
--   let game = LobbyState [PlayerSeat pid 1000]
--       evolved = evolve game (PlayerLeft pid)
--   case evolved of
--     LobbyState seats -> pid `notElem` map seatId seats
--     _ -> failure
