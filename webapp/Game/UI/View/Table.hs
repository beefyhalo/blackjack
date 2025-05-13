{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Game.UI.View.Table (viewTable) where

import Control.Monad.Writer.CPS (lift, tell)
import Data.Bool (bool)
import Data.Char (toLower)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Game.UI.Component (Component, items)
import Game.UI.Model (AnimationState (..), TableModel (..))
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core
import Types

viewTable :: Behavior TableModel -> Component
viewTable bTable = do
  dealerElem <- lift $ UI.div #. "dealer" # sink items (renderDealer <$> bTable)
  playersElem <- playersWidget (playerHands <$> bTable) (animation <$> bTable)

  lift $
    UI.div
      #. "table-container container mt-4"
      #+ [ UI.h3 #. "section-title" # set text "Dealer",
           element dealerElem,
           UI.h3 #. "section-title mt-4" # set text "Players",
           element playersElem
         ]

-- | Render the widget for all players
playersWidget :: Behavior (Map.Map PlayerId Hand) -> Behavior AnimationState -> Component
playersWidget bHands bAnim = do
  let find i = fmap (i,) . Map.lookup i
  playerElems <- traverse (\i -> playerWidget (fmap (find (PlayerId i)) bHands) bAnim) [0 .. 3]
  lift $ UI.div #. "players" # set children playerElems

-- | Render the single player's row
playerWidget :: Behavior (Maybe (PlayerId, Hand)) -> Behavior AnimationState -> Component
playerWidget bMaybePlayer bAnim = do
  let bHandElems = renderAnimatedHand <$> bAnim <*> (fmap snd <$> bMaybePlayer)
      bScoreText = ("Score: " <>) . foldMap (show . score . snd) <$> bMaybePlayer
      bPid = fmap fst <$> bMaybePlayer
      bNameText = foldMap show <$> bPid
      bBusted = maybe False (isBust . snd) <$> bMaybePlayer -- Check if hand is busted
      bStyle = bool "player mb-3" "player mb-3 busted" <$> bBusted

  controls <- controlsWidget bPid bBusted

  lift $
    UI.div
      # sink UI.class_ bStyle
      #+ [ UI.h5 #. "player-title mb-2" # sink text bNameText,
           UI.h5 #. "player-score mb-3" # sink text bScoreText,
           UI.div #. "hand d-flex flex-wrap gap-2" # sink items bHandElems,
           element controls
         ]

-- | Render the "Hit" and "Stand" buttons, emitting PlayerTurnCmd events
controlsWidget :: Behavior (Maybe PlayerId) -> Behavior Bool -> Component
controlsWidget bPid bBusted = do
  let enabled = (&&) <$> fmap isJust bPid <*> fmap not bBusted
  hitBtn <-
    lift $
      UI.button
        #. "btn btn-primary me-2"
        # set text "Hit Me"
        # sink UI.enabled enabled
  standBtn <-
    lift $
      UI.button
        #. "btn btn-secondary"
        # set text "Stand"
        # sink UI.enabled enabled

  let evHit = fmap (PlayerTurnCmd . Hit) <$> bPid <@ UI.click hitBtn
      evStand = fmap (PlayerTurnCmd . Stand) <$> bPid <@ UI.click standBtn
      evStandAndResolveRound = DealerTurnCmd DealerPlay <$ evStand

  tell [filterJust evHit, filterJust evStand, evStandAndResolveRound]

  lift $ UI.div #. "player-controls mt-2" #+ [element hitBtn, element standBtn]

-- | Dealer view with hole card and face-up card
renderDealer :: TableModel -> [UI Element]
renderDealer TableModel {dealer = Just (Dealer (Hand [card]))} =
  [renderCard card, renderCardBack]
renderDealer tableModel =
  renderAnimatedHand tableModel.animation (fmap dealerHand tableModel.dealer)

-- | Render a hand with optional animation
renderAnimatedHand :: AnimationState -> Maybe Hand -> [UI Element]
renderAnimatedHand anim = \case
  Just (Hand cards) -> case anim of
    AnimateDealing -> zipWith renderCardAnimated [0 ..] cards
    AnimateHit _pid -> map (renderCardAnimated 0) cards
    NoAnimation -> map renderCard cards
  Nothing -> []

-- | Render a card element with an optional animation delay
renderCardAnimated :: Int -> Card -> UI Element
renderCardAnimated i card =
  renderCard card #. ("card-img animate-deal delay-" ++ show i)

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
