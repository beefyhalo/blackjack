{-# LANGUAGE BlockArguments #-}

module Application (stateMachine, projection, whole) where

import Control.Arrow ((&&&))
import Crem.StateMachine (StateMachine, StateMachineT (Basic, Kleisli))
import Data.Foldable (fold)
import Data.List (singleton)
import Data.Profunctor (rmap)
import Domain
import Game (baseMachine)
import GameTopology (Decision)
import Projection (Summary, gameProjection)
import System.Random (StdGen)

stateMachine :: StdGen -> StateMachine Command Decision
stateMachine stdGen = Basic (baseMachine stdGen)

projection :: StateMachine Event Summary
projection = Basic gameProjection

whole :: StdGen -> StateMachine Command (Decision, Summary)
whole stdGen =
  let output = stateMachine stdGen
      writeModel = rmap (foldMap singleton) output
      readModel = rmap singleton projection
      summary = rmap fold (Kleisli writeModel readModel)
   in output &&& summary
