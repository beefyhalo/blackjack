{-# LANGUAGE BlockArguments #-}

module Game.UI.View.Table (viewTable) where

import Control.Monad.Writer.CPS (lift)
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

  dealerDiv <- UI.div # sink items (fmap renderDealer bTable)
  playerDiv <- UI.div # sink items (fmap renderPlayers bTable)

  UI.div
    #+ [ UI.h3 # set text "Dealer",
         element dealerDiv,
         UI.h3 # set text "Players",
         element playerDiv
       ]

renderPlayers :: TableModel -> [UI Element]
renderPlayers TableModel {playerHands} =
  fmap renderPlayer (Map.toList playerHands)

renderPlayer :: (PlayerId, Hand) -> UI Element
renderPlayer (PlayerId pid, hand) = do
  cardElems <- sequence $ renderHand hand
  UI.div #+ [UI.string ("Player " ++ show pid ++ ": "), UI.span # set children cardElems]

renderDealer :: TableModel -> [UI Element]
renderDealer TableModel {dealer} =
  renderHand . dealerHand =<< maybeToList dealer

renderHand :: Hand -> [UI Element]
renderHand (Hand cards) =
  [UI.span #+ map renderCard cards]

renderCard :: Card -> UI Element
renderCard Card {rank, suit} =
  UI.span # set text (show rank ++ " of " ++ show suit) # set style [("margin", "0 5px")]

items :: WriteAttr Element [UI Element]
items = mkWriteAttr $ \i x -> void do
  pure x # set children [] #+ map (\j -> UI.span #+ [j]) i
