{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Game.UI where

import Control.Monad.Identity (Identity (..))
import Crem.BaseMachine (runBaseMachineT)
import Data.Text qualified as Text
import Debug.Trace (traceShow)
import Domain
import Game (baseMachine)
import GameTopology (Decision, GamePhase (DealingCards, InLobby))
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core
import System.Random (StdGen, initStdGen)

data GameView = GameView
  { i :: Int,
    phase :: GamePhase
  }
  deriving (Show)

baseView :: GameView
baseView = GameView 0 InLobby

data Layout = Layout
  { controlPanel :: ControlPanel,
    tableBox :: Element
  }

data ControlPanel = ControlPanel
  { nameInput :: Element,
    joinButton :: Element,
    leaveButton :: Element,
    startButton :: Element,
    dealButton :: Element,
    container :: Element
  }

newLayout :: UI Layout
newLayout =
  Layout
    <$> newControlPanel
    <*> UI.div

newControlPanel :: UI ControlPanel
newControlPanel = do
  nameInput <- UI.input # set UI.text "Enter player name"
  joinBtn <- UI.button # set UI.text "Join Game"
  leaveBtn <- UI.button # set UI.text "Leave Game"
  startBtn <- UI.button # set UI.text "Start Game"
  dealBtn <- UI.button # set UI.text "Deal Cards"
  container <-
    UI.div
      #. "lobby-controls"
      #+ map element [nameInput, joinBtn, leaveBtn, startBtn, dealBtn]
  pure $ ControlPanel nameInput joinBtn leaveBtn startBtn dealBtn container

updateGameView :: Decision -> GameView -> GameView
updateGameView (Right (LobbyEvt GameStarted)) (GameView i x) = traceShow ("Woo!", x) (GameView (i + 1) DealingCards)
updateGameView d (GameView i x) = traceShow ("UPDATE GAME VIEWWWWWWW", d, x) (GameView (i + 1) x)

main :: IO ()
main = do
  stdGen <- initStdGen
  startGUI defaultConfig (setup stdGen)

setup :: StdGen -> Window -> UI ()
setup rng window = mdo
  -- Setup layout
  return window # set title "Blackjack"
  body <- getBody window
  layout <- newLayout
  element body #+ fmap element [layout.controlPanel.container]

  -- Game behavior setup
  (behaviorView, fireCommand) <- liftIO do
    (eCmd, fireCmd) <- newEvent
    (eDecision, _) <- mapAccum (baseMachine rng) (fmap (\x y -> runIdentity $ runBaseMachineT y x) eCmd)
    bView <- accumB baseView (fmap updateGameView eDecision)
    pure (bView, fireCmd)

  -- wire up reactive network
  wireUI layout behaviorView fireCommand

  pure ()

wireUI :: Layout -> Behavior GameView -> Handler Command -> UI ()
wireUI layout view fireCommand = do
  wireControl layout.controlPanel view fireCommand

--   element container # sink UI.enabled (fmap ((== InLobby) . phase) view)
--   pure ()

wireControl :: ControlPanel -> Behavior GameView -> Handler Command -> UI ()
wireControl ControlPanel {..} view fire = do
  on UI.click joinButton \_ -> do
    name <- get UI.value nameInput
    liftIO $ fire (LobbyCmd (JoinGame (Text.pack name)))
  on UI.click leaveButton \_ -> do
    _pid <- currentValue view
    liftIO $ fire (LobbyCmd (LeaveGame (PlayerId 0)))
  on UI.click startButton \_ -> liftIO $ fire (LobbyCmd StartGame)
  -- Only enable the deal button during DealingCards phase
  element dealButton # sink UI.enabled (fmap ((== DealingCards) . phase) view)

  -- Fire command when clicked
  on UI.click dealButton \_ -> liftIO $ fire (DealingCmd DealInitialCards)
