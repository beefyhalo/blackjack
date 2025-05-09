{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Policy (insurancePolicy, autoResolve) where

import Crem.BaseMachine (BaseMachine, BaseMachineT (..), InitialState (InitialState), pureResult, statelessBase)
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology (STopology (STopology), Topology (Topology), TopologySym0, TrivialTopology)
import Data.Set qualified as Set
import Types
import "singletons-base" Data.Singletons.Base.TH hiding (Sum)

$( singletons
     [d|
       data InsurancePolicyVertex = InsurancePolicyVertex
         deriving stock (Eq, Show, Enum, Bounded)

       insurancePolicyTopology :: Topology InsurancePolicyVertex
       insurancePolicyTopology = Topology []
       |]
 )

deriving via AllVertices InsurancePolicyVertex instance RenderableVertices InsurancePolicyVertex

data InsurancePolicyState (phase :: InsurancePolicyVertex) where
  InsurancePolicyState :: Set.Set PlayerId -> InsurancePolicyState 'InsurancePolicyVertex

insurancePolicy :: BaseMachine InsurancePolicyTopology Event (Maybe Command)
insurancePolicy =
  BaseMachineT
    { initialState = InitialState (InsurancePolicyState Set.empty),
      action = \(InsurancePolicyState pids) -> \case
        DealingEvt (CardsDealt ps _) -> pureResult Nothing (InsurancePolicyState $ Set.fromList (fmap fst ps))
        InsuranceEvt (PlayerTookInsurance pid _) -> emitResolveInsurance pid pids
        InsuranceEvt (PlayerDeclinedInsurance pid) -> emitResolveInsurance pid pids
        _ -> pureResult Nothing (InsurancePolicyState pids)
    }
  where
    emitResolveInsurance pid pids =
      let pids' = Set.delete pid pids
          command = if null pids' then Just (InsuranceCmd ResolveInsurance) else Nothing
       in pureResult command (InsurancePolicyState pids')

autoResolve :: BaseMachine (TrivialTopology @()) Event (Maybe Command)
autoResolve = statelessBase \case
  InsuranceEvt (InsuranceResolved _) -> resolveRound
  PlayerTurnEvt _ -> resolveRound
  DealerTurnEvt _ -> resolveRound
  _ -> Nothing
  where
    resolveRound = Just (ResolutionCmd ResolveRound)
