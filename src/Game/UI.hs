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

import Control.Applicative ((<*))
import Control.Monad (void)
import Control.Monad.Identity (Identity (..))
import Crem.BaseMachine (runBaseMachineT)
import Data.Foldable (for_, traverse_)
import Data.Map.Strict qualified as Map
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

data Model = Model
  { table :: TableView
  }
  deriving (Show)

data TableView = TableView
  { playerHands :: Map.Map PlayerId Hand,
    dealer :: Maybe Dealer
  }
  deriving (Show)

initialModel :: Model
initialModel = Model (TableView Map.empty Nothing)

main :: IO ()
main = startGUI defaultConfig setupGui

setupGui :: Window -> UI ()
setupGui window = mdo
  pure window # set title "Blackjack"

  (ui, eCommand) <- view model
  eDecision <- do
    let run x y = runIdentity (runBaseMachineT y x)
    rng <- initStdGen
    (result, _) <- mapAccum (baseMachine rng) (fmap run eCommand)
    pure result
  model <- accumB initialModel (fmap update eDecision)

  void $ getBody window #+ [element ui]

update :: Decision -> Model -> Model
update msg model = traceShow ("update", msg, model) $ case msg of
  Right (DealingEvt (CardsDealt ps dealer)) ->
    model {table = TableView (Map.fromList ps) (Just dealer)}
  _ -> model

view :: Behavior Model -> UI (Element, UI.Event Command)
view bModel = do
  (controlPanel, eCmd) <- viewControlPanel bModel
  tableView <- viewTable bModel

  root <- UI.div #+ [element controlPanel, UI.hr, element tableView]
  pure (root, eCmd)

viewControlPanel :: Behavior Model -> UI (Element, UI.Event Command)
viewControlPanel bModel = do
  txtName <- UI.input # set (attr "placeholder") "Enter name"
  btnJoin <- UI.button # set text "Join Game"
  btnLeave <- UI.button # set text "Leave Game"
  btnStart <- UI.button # set text "Start Game"
  txtBet <- UI.input # set (attr "type") "number" # set (attr "placeholder") "Bet amount"
  btnBet <- UI.button # set text "Place Bet"
  btnDeal <- UI.button # set text "Deal"

  nameIn <- stepper "" $ UI.valueChange txtName
  betIn <- stepper (Bet 0) $ maybe 0 Bet . readMaybe <$> UI.valueChange txtBet
  let evJoin = LobbyCmd . JoinGame . Text.pack <$> nameIn <@ UI.click btnJoin
      evLeave = LobbyCmd (LeaveGame (PlayerId 0)) <$ UI.click btnLeave
      evStart = LobbyCmd StartGame <$ UI.click btnStart
      evBet = BettingCmd . PlaceBet (PlayerId 0) <$> betIn <@ UI.click btnBet
      evDeal = DealingCmd DealInitialCards <$ UI.click btnDeal
      allEvents = unions [evJoin, evLeave, evStart, evBet, evDeal]

  root <-
    UI.div
      #+ [ UI.string "Lobby:",
           element txtName,
           element btnJoin,
           element btnLeave,
           element btnStart,
           UI.hr,
           UI.string "Betting:",
           element txtBet,
           element btnBet,
           element btnDeal
         ]

  pure (root, fmap head allEvents)

viewTable :: Behavior Model -> UI Element
viewTable bModel = do
  let bTable = table <$> bModel

  dealerDiv <- UI.div #+ [renderDealer =<< currentValue bTable]
  playerDiv <- UI.div # sink children (_)

  UI.div
    #+ [ UI.h3 # set text "Dealer",
         element dealerDiv,
         UI.h3 # set text "Players",
         element playerDiv
       ]

renderCard :: Card -> UI Element
renderCard Card {..} =
  UI.span # set text (show rank ++ " of " ++ show suit) # set style [("margin", "0 5px")]

renderPlayer :: (PlayerId, Hand) -> UI Element
renderPlayer (PlayerId pid, Hand cards) = do
  cardElems <- mapM renderCard cards
  UI.div #+ [UI.string ("Player " ++ show pid ++ ": "), UI.span #+ cardElems]

renderDealer :: TableView -> UI Element
renderDealer TableView {..} =
  UI.span #+ map renderCard dealer.dealerHand

renderPlayers :: TableView -> UI [Element]
renderPlayers TableView {..} =
  traverse renderPlayer (Map.toList playerHands)
