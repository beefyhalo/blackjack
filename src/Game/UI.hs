{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Game.UI where

import Control.Monad (void)
import Control.Monad.Identity (Identity (..))
import Crem.BaseMachine (BaseMachine, BaseMachineT (BaseMachineT, initialState), InitialState (InitialState), runBaseMachineT)
import Crem.StateMachine (run)
import Data.IORef
import Data.List (singleton)
import Data.Map.Strict qualified as Map
import Debug.Trace (trace, traceShow)
import Domain
import Game (baseMachine)
import GameTopology (Decision, Game (..), GameState (..), GameTopology, PlayerMap)
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core
import System.Random (StdGen, initStdGen)
import Text.Read (readMaybe)

data GameView = GameView Int deriving (Show)

baseView :: GameView
baseView = GameView 0

data Layout = Layout
  { lobbyBox :: Element,
    tableBox :: Element,
    controlBox :: Element,
    someButton :: Element,
    gameViewDisplay :: Element
  }

newLayout :: UI Layout
newLayout =
  Layout
    <$> UI.div
    <*> UI.div
    <*> UI.div
    <*> UI.button
    <*> UI.div

updateGameView :: Decision -> GameView -> GameView
updateGameView d (GameView i) = trace "UPDATE GAME VIEWWWWWWW" (GameView (i + 1))

main :: IO ()
main = do
  stdGen <- initStdGen
  startGUI defaultConfig (setup stdGen)

setup :: StdGen -> Window -> UI ()
setup rng window = mdo
  -- Setup layout
  return window # set title "Blackjack"
  body <- getBody window
  Layout {..} <- newLayout
  element body #+ fmap element [gameViewDisplay, someButton]

  -- Game behavior setup
  (behaviorView, fireCommand) <- liftIO $ do
    (eCmd, fireCmd) <- newEvent
    (eDecision, _) <- mapAccum (baseMachine rng) (fmap (\x y -> runIdentity $ runBaseMachineT y x) eCmd)
    bView <- accumB baseView (fmap updateGameView eDecision)
    pure (bView, fireCmd)

  -- wire up reactive network
  on UI.click someButton \_ -> liftIO $ fireCommand (LobbyCmd (JoinGame "Alice"))
  element gameViewDisplay # sink UI.text (fmap show behaviorView)

  pure ()