{-# LANGUAGE OverloadedLists #-}

module Game.UI.View.ControlPanel (viewControlPanel) where

import Control.Monad.Trans (lift)
import Control.Monad.Writer (tell)
import Data.Text (pack)
import Game.UI.Component (Component)
import Game.UI.Model (Model)
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core
import Text.Read (readMaybe)
import Types

viewControlPanel :: Behavior Model -> Component
viewControlPanel _bModel = do
  -- Input Fields
  txtName <- lift $ UI.input #. "input-name" # set (attr "placeholder") "Enter name"
  txtBet <- lift $ UI.input #. "input-bet" # set (attr "type") "number" # set (attr "placeholder") "Bet amount"

  -- Buttons
  btnJoin <- lift $ UI.button #. "btn" # set text "Join Game"
  btnLeave <- lift $ UI.button #. "btn" # set text "Leave Game"
  btnStart <- lift $ UI.button #. "btn" # set text "Start Game"
  btnBet <- lift $ UI.button #. "btn" # set text "Place Bet"
  btnDeal <- lift $ UI.button #. "btn" # set text "Deal"
  btnDealerTurn <- lift $ UI.button #. "btn" # set text "Dealer Turn"
  btnResolve <- lift $ UI.button #. "btn" # set text "Resolve"
  btnRestart <- lift $ UI.button #. "btn" # set text "Restart"

  -- Reactive Inputs
  nameIn <- stepper "" (UI.valueChange txtName)
  betIn <- stepper (Bet 0) $ maybe 0 Bet . readMaybe <$> UI.valueChange txtBet

  -- Event Wiring
  let evJoin = LobbyCmd . JoinGame . pack <$> nameIn <@ UI.click btnJoin
      evLeave = LobbyCmd (LeaveGame (PlayerId 0)) <$ UI.click btnLeave
      evStart = LobbyCmd StartGame <$ UI.click btnStart
      evBet = BettingCmd . PlaceBet (PlayerId 0) <$> betIn <@ UI.click btnBet
      evDeal = DealingCmd DealInitialCards <$ UI.click btnDeal
      evDealerTurn = DealerTurnCmd DealerPlay <$ UI.click btnDealerTurn
      evResolve = ResolutionCmd ResolveRound <$ UI.click btnResolve
      evRestart = ResultCmd RestartGame <$ UI.click btnRestart

  tell [evJoin, evLeave, evStart, evBet, evDeal, evDealerTurn, evResolve, evRestart]

  -- UI Layout
  lift $
    UI.div
      #. "control-panel"
      #+ [ UI.div
             #. "panel lobby-panel"
             #+ [ UI.h3 # set text "Lobby",
                  element txtName,
                  UI.div #. "button-row" #+ map element [btnJoin, btnLeave, btnStart]
                ],
           UI.hr,
           UI.div
             #. "panel betting-panel"
             #+ [ UI.h3 # set text "Betting",
                  element txtBet,
                  UI.div #. "button-row" #+ map element [btnBet, btnDeal]
                ],
           UI.hr,
           UI.div
             #. "panel game-panel"
             #+ [ UI.h3 # set text "Actions",
                  UI.div #. "button-row" #+ map element [btnDealerTurn, btnResolve, btnRestart]
                ]
         ]
