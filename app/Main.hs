module Main (main) where

import Application (gameLoop, whole)
import System.Random (initStdGen)

main :: IO ()
main = do
  putStrLn "Welcome to Blackjack!"
  stdGen <- initStdGen
  gameLoop (whole stdGen)