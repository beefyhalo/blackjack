{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Policy (insurancePolicy) where

import Crem.BaseMachine (BaseMachine, BaseMachineT (..), InitialState (InitialState), pureResult)
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology (STopology (STopology), Topology (Topology), TopologySym0)
import Data.Set qualified as Set
import Domain
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
