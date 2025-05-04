{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

module Game.UI.Model (module Game.UI.Model) where

import Data.Map.Strict qualified as Map
import Debug.Trace (traceShow)
import GameTopology (Decision)
import Types

newtype Model = Model
  { table :: TableModel
  }
  deriving (Show)

data TableModel = TableModel
  { playerHands :: Map.Map PlayerId Hand,
    dealer :: Maybe Dealer
  }
  deriving (Show)

initialModel :: Model
initialModel = Model (TableModel Map.empty Nothing)

update :: Decision -> Model -> Model
update msg model = traceShow ("update", msg, model) $ case msg of
  Right (DealingEvt (CardsDealt ps dealer)) ->
    model {table = TableModel (Map.fromList ps) (Just dealer)}
  Right (PlayerTurnEvt (HitCard pid card)) ->
    let playerHands = Map.adjust (addCard card) pid model.table.playerHands
     in model {table = model.table {playerHands}}
  _ -> model
