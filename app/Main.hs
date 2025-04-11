module Main (main) where

import FSM
import System.Random (initStdGen)

main :: IO ()
main = do
  stdGen <- initStdGen
  gameLoop (stateMachine stdGen)