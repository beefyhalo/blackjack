{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.StateTest.StateSpec (tests) where

import Application (stateMachine)
import Control.Monad.State.Strict (evalStateT)
import Game.StateTest.Model (initialModel)
import Game.StateTest.Lobby
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Random (initStdGen)

tests :: IO Bool
tests = checkSequential $$discover

prop_game_sequential :: Property
prop_game_sequential = property do
  actions <-
    forAll $
      Gen.sequential
        (Range.linear 1 100)
        initialModel
        commands
  stdGen <- initStdGen
  let machine = stateMachine stdGen
  evalStateT
    (executeSequential initialModel actions)
    machine
  where
    commands =
      [ joinCommand,
        leaveCommand,
        leaveGamePlayerNotFoundCommand,
        startGameCommand,
        startGameNotEnoughPlayersCommand,
        gameAlreadyStartedCommand
      ]
