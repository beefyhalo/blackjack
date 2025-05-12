{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GameTopology (module GameTopology) where

import Crem.Render.RenderableVertices
import Crem.Topology
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import System.Random (StdGen, split)
import Types
import "singletons-base" Data.Singletons.Base.TH

$( singletons
     [d|
       data GamePhase
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

       gameTopology :: Topology GamePhase
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

deriving via AllVertices GamePhase instance RenderableVertices GamePhase

data SomeGame = forall p. SomeGame (Game p)

data Game (phase :: GamePhase) = Game
  { stdGen :: StdGen,
    nextPlayerId :: Int,
    state :: GameState phase
  }
  deriving (Eq, Show)

withUpdatedRng :: Game v -> Game v
withUpdatedRng game = game {stdGen = let (_, g') = split (stdGen game) in g'}

data GameState (phase :: GamePhase) where
  LobbyState :: PlayerMap -> GameState 'InLobby
  BettingState :: PlayerMap -> GameState 'AwaitingBets
  DealingState :: PlayerMap -> Deck -> GameState 'DealingCards
  OfferingInsuranceState :: GameContext -> GameState 'OfferingInsurance
  ResolvingInsuranceState :: GameContext -> GameState 'ResolvingInsurance
  OpeningTurnState :: OpeningContext -> GameState 'OpeningTurn
  PlayerTurnState :: InsuranceContext -> GameState 'PlayerTurn
  DealerTurnState :: InsuranceContext -> GameState 'DealerTurn
  ResolvingState :: ResolutionContext -> GameState 'ResolvingHands
  ResultState :: PlayerMap -> GameState 'Result
  ExitedState :: GameState 'GameOver

deriving instance Eq (GameState phase)

deriving instance Show (GameState phase)

type PlayerMap = Map.Map PlayerId Player

data GameContext = GameContext
  { deck :: Deck,
    rounds :: Map.Map PlayerId PlayerRound,
    dealer :: Dealer
  }
  deriving (Eq, Show)

data InsuranceContext = InsuranceContext
  { context :: GameContext,
    insurancePayouts :: Map.Map PlayerId InsurancePayout
  }
  deriving (Eq, Show)

data OpeningContext = OpeningContext
  { insuranceContext :: InsuranceContext,
    readyPlayers :: Set.Set PlayerId
  }
  deriving (Eq, Show)

data ResolutionContext = ResolutionContext
  { resolvedRounds :: Map.Map PlayerId PlayerRound,
    resolvedDealer :: Dealer,
    resolvedInsurancePayouts :: Map.Map PlayerId InsurancePayout
  }
  deriving (Eq, Show)

type Decision = Either GameError Event
