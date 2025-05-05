{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Game.UI.View.ResultModal (viewResultModal) where

import Control.Monad.Writer.CPS (lift, tell)
import Data.Foldable (Foldable (toList))
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Game.UI.Component (Component, items)
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core
import Types

viewResultModal :: Behavior (Maybe ResolutionEvent) -> Component
viewResultModal bResult = do
  -- Buttons
  closeBtn <- lift $ UI.button #. "btn btn-secondary" # set text "Close"
  restartBtn <- lift $ UI.button #. "btn btn-primary" # set text "Play Again"
  exitBtn <- lift $ UI.button #. "btn btn-danger" # set text "Exit"

  let btns = [closeBtn, restartBtn, exitBtn]

  -- Event handlers for buttons
  let evRestart = ResultCmd RestartGame <$ UI.click restartBtn
      evExit = ExitGame <$ UI.click exitBtn

  tell [evRestart, evExit]

  -- Render modal based on result
  lift $ UI.div # sink items (fmap (maybeToList . fmap (renderModal btns)) bResult)

-- Rendering the modal content
renderModal :: [Element] -> ResolutionEvent -> UI Element
renderModal btns result = do
  modalBody <- renderModalBody result btns
  UI.div
    #. "modal fade show result-modal d-block"
    # set (attr "tabindex") "-1"
    #+ [ UI.div
           #. "modal-dialog modal-dialog-centered modal-lg"
           #+ [ UI.div
                  #. "modal-content"
                  # set children [modalBody]
              ]
       ]

-- Modal content and layout
renderModalBody :: ResolutionEvent -> [Element] -> UI Element
renderModalBody (RoundResolved dealerOutcome playerSummaries) btns = do
  dealerView <- renderDealerOutcome dealerOutcome
  playersView <- traverse (uncurry renderPlayerSummary) (Map.toList playerSummaries)

  UI.div
    #. "modal-body"
    #+ [ UI.div #. "dealer-section mb-3" #+ [element dealerView],
         UI.hr,
         UI.div #. "player-section mb-3" #+ map element playersView,
         UI.hr,
         UI.div #. "modal-footer d-flex justify-content-between" #+ map element btns
       ]

-- Rendering the dealer's outcome
renderDealerOutcome :: DealerOutcome -> UI Element
renderDealerOutcome outcome =
  UI.p
    #. "dealer-outcome h5"
    # set
      text
      ( "Dealer: " ++ case outcome of
          DealerBlackjack -> "Blackjack"
          DealerBust -> "Bust"
          DealerFinalScore s -> "Scored " ++ show s
      )

renderPlayerSummary :: PlayerId -> PlayerSummary -> UI Element
renderPlayerSummary pid summary = do
  let outcomesText = unlines (map outcomeText (toList summary.handOutcomes))
      chipChangeText = "Net change: " ++ show (netChipChange summary) ++ " chips"
      insuranceTxt = case summary.insurancePayout of
        Just p -> "Insurance: " ++ insuranceText p
        Nothing -> ""

  UI.div
    #. "player-summary card card-body mb-3"
    #+ [ UI.h5 #. "card-title" # set text ("Player " ++ show pid),
         UI.p #. "card-text" # set text outcomesText,
         UI.p #. "card-text" # set text chipChangeText,
         UI.p #. "card-text text-muted" # set text insuranceTxt
       ]

-- Helper to format outcome messages
outcomeText :: Outcome -> String
outcomeText = \case
  PlayerWins Blackjack -> "Win: Blackjack"
  PlayerWins OutscoredDealer -> "Win: Outscored Dealer"
  DealerWins PlayerBust -> "Loss: Player Bust"
  DealerWins Surrendered -> "Loss: Surrendered"
  DealerWins OutscoredByDealer -> "Loss: Dealer Outscored"
  Push -> "Push"

-- Helper to format insurance messages
insuranceText :: InsurancePayout -> String
insuranceText = \case
  WonInsurancePayout n -> "Won insurance: " ++ show n
  LostInsuranceBet n -> "Lost insurance: " ++ show n
  NoInsurance -> ""
