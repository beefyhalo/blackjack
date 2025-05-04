{-# LANGUAGE OverloadedLists #-}

module Game.UI.View.ControlPanel (viewControlPanel) where

import Control.Monad.Trans (lift)
import Control.Monad.Writer (tell)
import Data.Text (pack)
import Game.UI.Component (Component)
import Game.UI.Model (Model)
import Graphics.UI.Threepenny
import Graphics.UI.Threepenny qualified as UI
import Text.Read (readMaybe)
import Types

viewControlPanel :: Behavior Model -> Component
viewControlPanel _bModel = do
  txtName <- lift $ UI.input # set (attr "placeholder") "Enter name"
  btnJoin <- lift $ UI.button # set text "Join Game"
  btnLeave <- lift $ UI.button # set text "Leave Game"
  btnStart <- lift $ UI.button # set text "Start Game"
  txtBet <- lift $ UI.input # set (attr "type") "number" # set (attr "placeholder") "Bet amount"
  btnBet <- lift $ UI.button # set text "Place Bet"
  btnDeal <- lift $ UI.button # set text "Deal"
  btnDealerTurn <- lift $ UI.button # set text "Dealer Turn"
  btnResolve <- lift $ UI.button # set text "Resolve"
  btnRestart <- lift $ UI.button # set text "Restart"

  nameIn <- stepper "" (UI.valueChange txtName)
  betIn <- stepper (Bet 0) $ maybe 0 Bet . readMaybe <$> UI.valueChange txtBet
  let evJoin = LobbyCmd . JoinGame . pack <$> nameIn <@ UI.click btnJoin
      evLeave = LobbyCmd (LeaveGame (PlayerId 0)) <$ UI.click btnLeave
      evStart = LobbyCmd StartGame <$ UI.click btnStart
      evBet = BettingCmd . PlaceBet (PlayerId 0) <$> betIn <@ UI.click btnBet
      evDeal = DealingCmd DealInitialCards <$ UI.click btnDeal
      evDealerTurn = DealerTurnCmd DealerPlay <$ UI.click btnDealerTurn
      evResolve = ResolutionCmd ResolveRound <$ UI.click btnResolve
      evRestart = ResultCmd RestartGame <$ UI.click btnRestart

  tell [evJoin, evLeave, evStart, evBet, evDeal, evDealerTurn, evResolve, evRestart]

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
           element btnDeal,
           UI.hr,
           element btnDealerTurn,
           element btnResolve,
           element btnRestart
         ]
