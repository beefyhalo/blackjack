{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import Control.Monad (void)
import Control.Monad.Identity (Identity (..))
import Crem.BaseMachine (runBaseMachineT)
import Game (baseMachine)
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
  let initialGame = baseMachine rng

  -- Reactive Model-Update-View
  (ui, EventStream commands) <- runComponent (view model)
  (decisions, _) <- mapAccum initialGame (fmap runGame commands)
  model <- accumB initialModel (fmap update decisions)

  getBody window # set children [ui]
  where
    runGame command game = runIdentity (runBaseMachineT game command)
