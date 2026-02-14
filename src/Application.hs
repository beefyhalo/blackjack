module Application (stateMachine, stateMachineWithAuto, stateMachineWithPolicy, projection, whole) where

import Control.Arrow ((&&&))
import Crem.BaseMachine (eitherM)
import Crem.StateMachine (StateMachine, StateMachineT (..))
import Data.Either (fromRight)
import Data.Foldable (fold)
import Data.List (singleton)
import Data.Profunctor (rmap)
import Game (baseMachine)
import GameTopology (Decision)
import Policy (autoResolve, insurancePolicy)
import Projection (Summary, gameProjection)
import System.Random (StdGen)
import Types

stateMachine :: StdGen -> StateMachine Command Decision
stateMachine stdGen = Basic (baseMachine stdGen)

policy :: StateMachine Event (Maybe Command)
policy = Basic insurancePolicy

autoPolicy :: StateMachine Decision [Command]
autoPolicy = Basic $ rmap (fromRight []) (eitherM autoResolve)

stateMachineWithAuto :: StdGen -> StateMachine Command [Decision]
stateMachineWithAuto stdGen = Feedback stateMachine' autoPolicy
  where
    stateMachine' = rmap singleton (stateMachine stdGen)

stateMachineWithPolicy :: StdGen -> StateMachine Command [Event]
stateMachineWithPolicy stdGen = Feedback stateMachine' policy'
  where
    stateMachine' = rmap (foldMap singleton) (stateMachine stdGen)
    policy' = rmap (foldMap singleton) policy

projection :: StateMachine Event Summary
projection = Basic gameProjection

whole :: StdGen -> StateMachine Command (Decision, Summary)
whole stdGen = output &&& summary
  where
    output = stateMachine stdGen
    writeModel = rmap (foldMap singleton) output
    readModel = rmap singleton projection
    summary = rmap fold (Kleisli writeModel readModel)
