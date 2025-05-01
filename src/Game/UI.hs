{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}

module Game.UI (main) where

import Control.Monad (void)
import Control.Monad.Identity (Identity (..))
import Crem.BaseMachine (runBaseMachineT)
import Game (baseMachine)
import Game.UI.Component (EventStream (..), runComponent)
import Game.UI.Model (initialModel, update)
import Game.UI.View (view)
import Graphics.UI.Threepenny (addStyleSheet)
import Graphics.UI.Threepenny.Core
import System.Random (initStdGen)

main :: IO ()
main = startGUI defaultConfig setupGui

setupGui :: Window -> UI ()
setupGui window = void mdo
  _ <- pure window # set title "Blackjack"
  addStyleSheet window "style.css"
  rng <- initStdGen
  let initialGame = baseMachine rng

  -- Reactive Model-Update-View
  (ui, EventStream commands) <- runComponent (view model)
  (decisions, _) <- mapAccum initialGame (fmap runGame commands)
  model <- accumB initialModel (fmap update decisions)

  getBody window #+ [element ui]
  where
    runGame command game = runIdentity (runBaseMachineT game command)
