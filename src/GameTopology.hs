{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GameTopology where

import Crem.Render.RenderableVertices
import Crem.Topology
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Domain
import System.Random (StdGen, split)
import "singletons-base" Data.Singletons.Base.TH

$( singletons
     [d|
       data GameVertex
         = InLobby
         | AwaitingBets
         | DealingCards
         | OfferingInsurance
         | ResolvingInsurance
         | OpeningTurn
         | PlayerTurn
         | DealerTurn
         | ResolvingHands
         | Result
         | GameOver
         deriving stock (Eq, Show, Enum, Bounded)

       gameTopology :: Topology GameVertex
       gameTopology =
         Topology
           [ (InLobby, [AwaitingBets]),
             (AwaitingBets, [DealingCards]),
             (DealingCards, [OfferingInsurance, OpeningTurn]),
             (OfferingInsurance, [ResolvingInsurance]),
             (ResolvingInsurance, [OpeningTurn, ResolvingHands]),
             (OpeningTurn, [PlayerTurn, DealerTurn, ResolvingHands]),
             (PlayerTurn, [DealerTurn, ResolvingHands]),
             (DealerTurn, [ResolvingHands]),
             (ResolvingHands, [Result]),
             (Result, [InLobby, GameOver])
           ]
       |]
 )

deriving via AllVertices GameVertex instance RenderableVertices GameVertex

data Game (vertex :: GameVertex) = Game
  { stdGen :: StdGen,
    state :: GameState vertex
  }

withUpdatedRng :: Game v -> Game v
withUpdatedRng game = game {stdGen = let (_, g') = split (stdGen game) in g'}

data GameState (vertex :: GameVertex) where
  LobbyState :: Map.Map PlayerId PlayerSeat -> GameState 'InLobby
  BiddingState :: Map.Map PlayerId PlayerSeat -> GameState 'AwaitingBets
  DealingState :: Map.Map PlayerId PlayerSeat -> Deck -> GameState 'DealingCards
  OfferingInsuranceState :: Deck -> Map.Map PlayerId Player -> Dealer -> GameState 'OfferingInsurance
  ResolvingInsuranceState :: Deck -> Map.Map PlayerId Player -> Dealer -> GameState 'ResolvingInsurance
  OpeningTurnState :: Deck -> Map.Map PlayerId Player -> Dealer -> Set.Set PlayerId -> GameState 'OpeningTurn
  PlayerTurnState :: Deck -> Map.Map PlayerId Player -> Dealer -> GameState 'PlayerTurn
  DealerTurnState :: Deck -> Map.Map PlayerId Player -> Dealer -> GameState 'DealerTurn
  ResolvingState :: Map.Map PlayerId Player -> Dealer -> GameState 'ResolvingHands
  ResultState :: Map.Map PlayerId PlayerSeat -> GameState 'Result
  ExitedState :: GameState 'GameOver

type Decision = Either GameError Event
