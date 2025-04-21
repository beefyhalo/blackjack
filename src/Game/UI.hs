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

import Control.Monad (join, void)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Writer (MonadTrans (lift), WriterT (runWriterT), tell)
import Crem.BaseMachine (runBaseMachineT)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Text qualified as Text
import Debug.Trace (traceShow)
import Domain
import Game (baseMachine)
import GameTopology (Decision)
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core
import System.Random (initStdGen)
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

  (ui, eCommands) <- runWriterT (view model)
  let eCommand = fmap head (unions eCommands)
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

view :: Behavior Model -> WriterT [UI.Event Command] UI Element
view bModel = do
  controlPanel <- viewControlPanel bModel
  tableView <- viewTable bModel

  lift $ UI.div #+ [element controlPanel, UI.hr, element tableView]

viewControlPanel :: Behavior Model -> WriterT [UI.Event Command] UI Element
viewControlPanel bModel = do
  txtName <- lift $ UI.input # set (attr "placeholder") "Enter name"
  btnJoin <- lift $ UI.button # set text "Join Game"
  btnLeave <- lift $ UI.button # set text "Leave Game"
  btnStart <- lift $ UI.button # set text "Start Game"
  txtBet <- lift $ UI.input # set (attr "type") "number" # set (attr "placeholder") "Bet amount"
  btnBet <- lift $ UI.button # set text "Place Bet"
  btnDeal <- lift $ UI.button # set text "Deal"

  nameIn <- stepper "" $ UI.valueChange txtName
  betIn <- stepper (Bet 0) $ maybe 0 Bet . readMaybe <$> UI.valueChange txtBet
  let evJoin = LobbyCmd . JoinGame . Text.pack <$> nameIn <@ UI.click btnJoin
      evLeave = LobbyCmd (LeaveGame (PlayerId 0)) <$ UI.click btnLeave
      evStart = LobbyCmd StartGame <$ UI.click btnStart
      evBet = BettingCmd . PlaceBet (PlayerId 0) <$> betIn <@ UI.click btnBet
      evDeal = DealingCmd DealInitialCards <$ UI.click btnDeal

  tell [evJoin, evLeave, evStart, evBet, evDeal]

  lift $
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

viewTable :: Behavior Model -> WriterT [UI.Event Command] UI Element
viewTable bModel = lift do
  let bTable = table <$> bModel

  dealerDiv <- UI.div # sink items (fmap renderDealer bTable)
  playerDiv <- UI.div # sink items (fmap renderPlayers bTable)

  UI.div
    #+ [ UI.h3 # set text "Dealer",
         element dealerDiv,
         UI.h3 # set text "Players",
         element playerDiv
       ]

renderCard :: Card -> UI Element
renderCard Card {..} =
  UI.span # set text (show rank ++ " of " ++ show suit) # set style [("margin", "0 5px")]

renderHand :: Hand -> [UI Element]
renderHand (Hand cards) =
  [UI.span #+ map renderCard cards]

renderPlayer :: (PlayerId, Hand) -> UI Element
renderPlayer (PlayerId pid, hand) = do
  cardElems <- sequence $ renderHand hand
  UI.div #+ [UI.string ("Player " ++ show pid ++ ": "), UI.span # set children cardElems]

renderPlayers :: TableView -> [UI Element]
renderPlayers TableView {..} = do
  fmap renderPlayer (Map.toList playerHands)

renderDealer :: TableView -> [UI Element]
renderDealer TableView {..} =
  let hands = maybeToList $ fmap dealerHand dealer
   in join $ traverse renderHand hands

items :: WriteAttr Element [UI Element]
items = mkWriteAttr $ \i x -> void $ do
  pure x # set children [] #+ map (\j -> UI.span #+ [j]) i
