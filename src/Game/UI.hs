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
import Data.Foldable (for_, traverse_)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Traversable (for)
import Debug.Trace (traceShow, traceShowM)
import Domain
import Game (baseMachine)
import GameTopology (Decision, GamePhase (AwaitingBets, DealingCards, InLobby))
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core
import System.Random (StdGen, initStdGen)
import Text.Read (readMaybe)

data GameView = GameView
  { i :: Int,
    phase :: GamePhase,
    currentPlayer :: Maybe PlayerId
  }
  deriving (Show)

baseView :: GameView
baseView = GameView 0 InLobby Nothing

data Layout = Layout
  { controlPanel :: ControlPanel,
    tableBox :: Element
  }

newLayout :: UI Layout
newLayout =
  Layout
    <$> newControlPanel
    <*> UI.div

data ControlPanel = ControlPanel
  { nameInput :: Element,
    joinButton :: Element,
    leaveButton :: Element,
    startButton :: Element,
    dealButton :: Element,
    betInput :: Element,
    betButton :: Element,
    container :: Element
  }

newControlPanel :: UI ControlPanel
newControlPanel = do
  nameInput <- UI.input # set UI.text "Enter player name"
  joinBtn <- UI.button # set UI.text "Join Game"
  leaveBtn <- UI.button # set UI.text "Leave Game"
  startBtn <- UI.button # set UI.text "Start Game"
  dealBtn <- UI.button # set UI.text "Deal Cards"
  betInput <- UI.input # set (UI.attr "type") "number" # set (UI.attr "placeholder") "Bet amount"
  betBtn <- UI.button # set UI.text "Place Bet"
  container <-
    UI.div
      #. "control-panel"
      #+ map
        element
        [ nameInput,
          joinBtn,
          leaveBtn,
          startBtn,
          betInput,
          betBtn,
          dealBtn
        ]

  pure $ ControlPanel nameInput joinBtn leaveBtn startBtn dealBtn betInput betBtn container

updateGameView :: Decision -> GameView -> GameView
updateGameView (Right (LobbyEvt (PlayerJoined pid _))) g = g {currentPlayer = Just pid}
updateGameView (Right (LobbyEvt GameStarted)) (GameView i x u) = traceShow ("Woo!", x) (GameView (i + 1) AwaitingBets u)
updateGameView d (GameView i x y) = traceShow ("UPDATE GAME VIEWWWWWWW", d, x) (GameView (i + 1) x y)

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

wireControl :: ControlPanel -> Behavior GameView -> Handler Command -> UI ()
wireControl ControlPanel {..} view fire = do
  -- Extract phase behavior
  let bPhase = fmap phase view

  on UI.click joinButton \_ -> do
    name <- get UI.value nameInput
    liftIO $ fire (LobbyCmd (JoinGame (Text.pack name)))
  on UI.click leaveButton \_ -> do
    _pid <- currentValue view
    liftIO $ fire (LobbyCmd (LeaveGame (PlayerId 0)))
  on UI.click startButton \_ ->
    liftIO $ fire (LobbyCmd StartGame)
  on UI.click betButton \_ -> do
    v <- get UI.value betInput
    case readMaybe v of
      Just n | n > 0 -> liftIO do
        pidMay <- fmap currentPlayer (currentValue view)
        for_ pidMay \pid -> fire (BettingCmd (PlaceBet pid (Bet n)))
      _ -> return () -- optionally show an error later
  on UI.click dealButton \_ -> liftIO $ fire (DealingCmd DealInitialCards)

  -- Enable controls only in certain phases
  element joinButton # sink UI.enabled (isPhase InLobby <$> bPhase)
  element leaveButton # sink UI.enabled (isPhase InLobby <$> bPhase)
  element startButton # sink UI.enabled (isPhase InLobby <$> bPhase)
  element nameInput # sink UI.enabled (isPhase InLobby <$> bPhase)

  element betInput # sink UI.enabled (isPhase AwaitingBets <$> bPhase)
  element betButton # sink UI.enabled (isPhase AwaitingBets <$> bPhase)

  element dealButton # sink UI.enabled (isPhase DealingCards <$> bPhase)

  pure ()

isPhase :: GamePhase -> GamePhase -> Bool
isPhase expected actual = expected == actual
