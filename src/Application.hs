{-# LANGUAGE BlockArguments #-}

module Application (gameLoop, stateMachine) where

import Crem.StateMachine (StateMachine, StateMachineT (Basic), run)
import Data.Functor.Identity (Identity (Identity))
import Domain (Command, Event, GameError)
import Game (baseMachine)
import System.IO.Error (catchIOError)
import System.Random (StdGen)

stateMachine :: StdGen -> StateMachine Command (Either GameError Event)
stateMachine g = Basic (baseMachine g)

gameLoop :: StateMachineT Identity Command (Either GameError Event) -> IO ()
gameLoop machine = do
  command <- commandLoop
  let Identity (output, machine') = run machine command
  print output
  gameLoop machine'

commandLoop :: IO Command
commandLoop = catchIOError readLn $ const do
  putStrLn "Invalid Command."
  commandLoop