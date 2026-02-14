{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}

module Main (main) where

import Application (stateMachineWithAuto)
import Control.Monad (void)
import Control.Monad.Identity (Identity (..))
import Crem.StateMachine (runMultiple)
import Game.UI.Component (EventStream (..), runComponent)
import Game.UI.Model (initialModel, update)
import Game.UI.View (view)
import Graphics.UI.Threepenny.Core
import System.Random (initStdGen)

main :: IO ()
main = startGUI config setupGui
  where
    config =
      defaultConfig
        { jsStatic = Just "static",
          jsCustomHTML = Just "index.html"
        }

setupGui :: Window -> UI ()
setupGui window = void mdo
  rng <- initStdGen
  let initialGame = stateMachineWithAuto rng

  -- Reactive Model-Update-View
  (ui, EventStream commands) <- runComponent (view model)
  (decisions, _) <- mapAccum initialGame (runGame <$> commands)
  model <- accumB initialModel (flip (foldr update) <$> decisions)

  getBody window # set children [ui]
  where
    runGame command game = runIdentity (runMultiple game command)
