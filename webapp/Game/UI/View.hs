module Game.UI.View (view) where

import Control.Monad.Trans (lift)
import Game.UI.Component (Component)
import Game.UI.Model (Model (..))
import Game.UI.View.ControlPanel (viewControlPanel)
import Game.UI.View.ResultModal (viewResultModal)
import Game.UI.View.Table (viewTable)
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core

view :: Behavior Model -> Component
view bModel = do
  controlPanel <- viewControlPanel bModel
  tableView <- viewTable (fmap table bModel)
  resultModal <- viewResultModal (fmap result bModel)

  lift $ UI.div #+ [element controlPanel, UI.hr, element tableView, element resultModal]
