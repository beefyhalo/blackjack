{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Game.UI.View.Table (viewTable) where

import Control.Monad.Writer.CPS (lift)
import Data.Char (toLower)
import Data.Functor (void)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Game.UI.Component (Component)
import Game.UI.Model (Model (..), TableModel (..))
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core
import Types

viewTable :: Behavior Model -> Component
viewTable bModel = lift do
  let bTable = fmap table bModel

  dealerDiv <- UI.div #. "dealer" # sink items (fmap renderDealer bTable)
  playerDiv <- UI.div #. "players" # sink items (fmap renderPlayers bTable)

  UI.div
    #. "table-container container mt-4"
    #+ [ UI.h3 #. "section-title" # set text "Dealer",
         element dealerDiv,
         UI.h3 #. "section-title mt-4" # set text "Players",
         element playerDiv
       ]

renderPlayers :: TableModel -> [UI Element]
renderPlayers TableModel {playerHands} =
  fmap renderPlayer (Map.toList playerHands)

renderPlayer :: (PlayerId, Hand) -> UI Element
renderPlayer (PlayerId pid, hand) = do
  cardElems <- sequence $ renderHand hand
  UI.div
    #. "player mb-3"
    #+ [ UI.h5 #. "player-title mb-2" # set text ("Player " ++ show pid),
         UI.div #. "hand d-flex flex-wrap gap-2" # set children cardElems
       ]

renderDealer :: TableModel -> [UI Element]
renderDealer TableModel {dealer} =
  renderHand . dealerHand =<< maybeToList dealer

renderHand :: Hand -> [UI Element]
renderHand (Hand cards) =
  map renderCard cards

renderCard :: Card -> UI Element
renderCard card =
  UI.img
    #. "card-img"
    # set UI.src ("/static/images/cards/fronts/" ++ cardImageName card ++ ".svg")
    # set UI.alt (show card.rank ++ " of " ++ show card.suit)

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
