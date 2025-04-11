{-# LANGUAGE BlockArguments #-}

module Application (gameLoop, stateMachine, projection, whole) where

import Control.Arrow ((&&&))
import Crem.StateMachine (StateMachine, StateMachineT (Basic, Kleisli), run)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity (Identity))
import Data.List (singleton)
import Data.Profunctor (rmap)
import Domain (Command, Event, GameError)
import Game (baseMachine)
import Projection (Summary, gameProjection)
import System.IO.Error (catchIOError)
import System.Random (StdGen)

stateMachine :: StdGen -> StateMachine Command (Either GameError Event)
stateMachine stdGen = Basic (baseMachine stdGen)

projection :: StateMachine Event Summary
projection = Basic gameProjection

whole :: StdGen -> StateMachine Command (Either GameError Event, Summary)
whole stdGen =
  let output = stateMachine stdGen
      writeModel = rmap (foldMap singleton) output
      readModel = rmap singleton projection
      summary = rmap fold (Kleisli writeModel readModel)
   in output &&& summary

gameLoop :: (Show output) => StateMachineT Identity Command output -> IO ()
gameLoop machine = do
  command <- commandLoop
  let Identity (output, machine') = run machine command
  print output
  gameLoop machine'

commandLoop :: IO Command
commandLoop = catchIOError readLn $ const do
  putStrLn "Invalid Command."
  commandLoop