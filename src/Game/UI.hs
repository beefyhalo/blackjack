module Game.UI () where

-- import Control.Monad.Fix (MonadFix)
-- import Data.Text (Text, pack)
-- import Reflex.Dom

-- main :: IO ()
-- main = mainWidget blackjackUI

-- blackjackUI :: (MonadWidget t m, MonadFix m) => m ()
-- blackjackUI = do
--   el "h2" $ text "♠️ Blackjack"

--   -- Event stream from buttons
--   hitEvent <- button "Hit"
--   standEvent <- button "Stand"

--   let commandEvent = leftmost [Hit <$ hitEvent, Stand <$ standEvent]

--   -- Reactive game state
--   gameStateDyn <- foldDyn applyCommand (Playing 15) commandEvent

--   -- Show current game state
--   dynText =<< mapDyn (pack . show) gameStateDyn

--   -- Show game result if terminal
--   let resultText gs = case gs of
--         Blackjack -> Just "🎉 Blackjack! You win!"
--         Busted -> Just "💥 Busted! You lose."
--         _ -> Nothing

--   widgetHold blank $ fmap (maybe blank (el "p" . text)) (updated $ resultText <$> gameStateDyn)
