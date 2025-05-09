{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}

module Game.UI.Component (module Game.UI.Component) where

import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT)
import Data.Functor (void)
import GHC.IsList
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny (onChange)
import Types

newtype EventStream = EventStream {events :: UI.Event [Command]}

instance IsList EventStream where
  type Item EventStream = UI.Event Command
  fromList = EventStream . UI.unions
  toList _ = []

instance Semigroup EventStream where
  EventStream e1 <> EventStream e2 =
    EventStream (UI.unionWith (++) e1 e2)

instance Monoid EventStream where
  mempty = EventStream UI.never

type Component = WriterT EventStream UI.UI UI.Element

runComponent :: Component -> UI.UI (UI.Element, EventStream)
runComponent = runWriterT

-- | Replace all children of an element with the given elements
items :: WriteAttr Element [UI Element]
items = mkWriteAttr $ \i el -> void do element el # set children [] #+ i

-- | Like `onChanges` but deferred by liftIOLater
onChangesLater :: Behavior a -> (a -> UI ()) -> UI ()
onChangesLater behavior f = do
  w <- askWindow
  liftIOLater $ onChange behavior (runUI w . f)
