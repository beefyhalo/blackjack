module Main (main) where

import Application (gameLoop, stateMachine)
import System.Random (initStdGen)

main :: IO ()
main = do
  stdGen <- initStdGen
  gameLoop (stateMachine stdGen)