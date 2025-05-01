{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Game.UI.View.Table (viewTable) where

import Control.Monad.Writer.CPS (lift, tell)
import Data.Char (toLower)
import Data.Functor (void)
import Data.Map.Strict qualified as Map
import Game.UI.Component (Component)
import Game.UI.Model (TableModel (..))
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core
import Types

viewTable :: Behavior TableModel -> Component
viewTable bTable = do
  dealerDiv <- lift $ UI.div #. "dealer" # sink items (fmap renderDealer bTable)

  playerDiv <- lift do
    -- let playerIds = fmap (Map.keys . playerHands) bTable
    -- let playerControlElems = fmap renderPlayerControls <$> playerIds
    UI.div
      #. "players"
      #+ [ UI.div # sink items (fmap renderPlayers bTable)
      --  UI.div #. "player-controls mt-4 d-flex gap-3" # sink items playerControlElems
         ]

  lift $
    UI.div
      #. "table-container container mt-4"
      #+ [ UI.h3 #. "section-title" # set text "Dealer",
           element dealerDiv,
           UI.h3 #. "section-title mt-4" # set text "Players",
           element playerDiv
         ]

renderPlayers :: TableModel -> [UI Element]
renderPlayers TableModel {playerHands} =
  map renderPlayer (Map.toList playerHands)

renderPlayer :: (PlayerId, Hand) -> UI Element
renderPlayer (pid, hand) = do
  cardElems <- sequence $ renderHand hand
  UI.div
    #. "player mb-3"
    #+ [ UI.h5 #. "player-title mb-2" # set text (show pid),
         UI.div #. "hand d-flex flex-wrap gap-2" # set children cardElems
       ]

renderPlayerControls :: PlayerId -> Component
renderPlayerControls pid = do
  hitBtn <- lift $ UI.button #. "btn btn-primary me-2" # set text "Hit Me"
  standBtn <- lift $ UI.button #. "btn btn-secondary" # set text "Stand"

  let evHit = PlayerTurnCmd (Hit pid) <$ UI.click hitBtn
      evStand = PlayerTurnCmd (Stand pid) <$ UI.click standBtn

  tell [evHit, evStand]

  lift $ UI.div #. "player-controls mt-2" #+ [element hitBtn, element standBtn]

renderDealer :: TableModel -> [UI Element]
renderDealer table = case table.dealer of
  Just dealer -> [renderCardBack, renderCard (visibleCard dealer)]
  _ -> []

renderHand :: Hand -> [UI Element]
renderHand (Hand cards) =
  map renderCard cards

renderCard :: Card -> UI Element
renderCard card =
  UI.img
    #. "card-img"
    # set UI.src ("/static/images/cards/fronts/" ++ cardImageName card ++ ".svg")
    # set UI.alt (show card.rank ++ " of " ++ show card.suit)

renderCardBack :: UI Element
renderCardBack =
  UI.img
    #. "card-img"
    # set UI.src "/static/images/cards/backs/abstract.svg"
    # set UI.alt "Face-down card"

cardImageName :: Card -> String
cardImageName Card {rank, suit} =
  toLower <$> show suit ++ "_" ++ showRank rank
  where
    showRank = \case
      Two -> "2"
      Three -> "3"
      Four -> "4"
      Five -> "5"
      Six -> "6"
      Seven -> "7"
      Eight -> "8"
      Nine -> "9"
      Ten -> "10"
      _ -> show rank

items :: WriteAttr Element [UI Element]
items = mkWriteAttr $ \i x -> void do
  pure x # set children [] #+ map (\j -> UI.span #+ [j]) i
