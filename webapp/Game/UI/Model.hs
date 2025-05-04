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
    dealer :: Maybe Dealer,
    animation :: AnimationState
  }
  deriving (Show)

data AnimationState
  = NoAnimation
  | AnimateDealing
  | AnimateHit PlayerId
  deriving (Show, Eq)

initialModel :: Model
initialModel = Model (TableModel Map.empty Nothing NoAnimation)

update :: Decision -> Model -> Model
update msg model = traceShow ("update", msg, model) $ case msg of
  Right (DealingEvt (CardsDealt ps dealer)) ->
    model {table = TableModel (Map.fromList ps) (Just $ Dealer (Hand [visibleCard dealer])) AnimateDealing}
  Right (PlayerTurnEvt (HitCard pid card)) ->
    let playerHands = Map.adjust (addCard card) pid model.table.playerHands
     in model {table = model.table {playerHands, animation = AnimateHit pid}}
  Right (DealerTurnEvt (DealerPlayed dealer)) ->
    model {table = model.table {dealer = Just dealer}}
  _ -> model {table = model.table {animation = NoAnimation}}
