{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Application (whole)
import Crem.StateMachine (StateMachineT, run)
import Data.Functor.Identity (Identity (Identity))
import Types (Command)
import System.IO.Error (catchIOError)
import System.Random (initStdGen)

main :: IO ()
main = do
  putStrLn "Welcome to Blackjack!"
  stdGen <- initStdGen
  gameLoop (whole stdGen)

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
