{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Projection (module Projection) where

import Crem.BaseMachine (BaseMachine, BaseMachineT (..), InitialState (InitialState), pureResult)
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology (STopology (STopology), Topology (Topology), TopologySym0)
import Data.Monoid (Sum)
import Data.Override (Override (Override))
import Domain (Chips, Event (RoundResolved))
import GHC.Generics (Generic)
import "singletons-base" Data.Singletons.Base.TH hiding (Sum)

data Summary = Summary
  { winnings :: Sum Chips,
    losses :: Sum Chips,
    rounds :: Sum Int
  }
  deriving (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via Override Summary '[]

$( singletons
     [d|
       data ProjectionVertex = SingleProjectionVertex
         deriving stock (Eq, Show, Enum, Bounded)

       projectionTopology :: Topology ProjectionVertex
       projectionTopology = Topology []
       |]
 )

deriving via AllVertices ProjectionVertex instance RenderableVertices ProjectionVertex

data ProjectionState (vertex :: ProjectionVertex) where
  SingleProjectionState :: Summary -> ProjectionState 'SingleProjectionVertex

gameProjection :: BaseMachine ProjectionTopology Event Summary
gameProjection =
  BaseMachineT
    { initialState = InitialState (SingleProjectionState mempty),
      action = \(SingleProjectionState summary) -> \case
        RoundResolved _ outcomes ->
          let winnings = undefined outcomes -- foldMap (\case (Win chips, _) -> Sum chips; _ -> 0) outcomes
              losses = undefined outcomes -- foldMap (\case (Loss chips, _) -> Sum chips; _ -> 0) outcomes
              summary' = summary <> Summary winnings losses 1
           in pureResult summary' (SingleProjectionState summary')
        _ -> pureResult summary (SingleProjectionState summary)
    }
