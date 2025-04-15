{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game (baseMachine, decider) where

import Crem.BaseMachine (BaseMachine, InitialState (..))
import Crem.Decider (Decider (..), EvolutionResult (EvolutionResult), deciderMachine)
import Domain (Command (..), Event (GameExited))
import Game.Betting
import Game.DealerTurn
import Game.Dealing
import Game.Insurance
import Game.Lobby
import Game.PlayerTurn
import Game.Resolution
import Game.Result
import GameTopology
import System.Random (StdGen)

baseMachine :: StdGen -> BaseMachine GameTopology Command Decision
baseMachine stdGen = deciderMachine (decider (InitialState (Game stdGen 0 (LobbyState mempty))))

decider :: InitialState Game -> Decider GameTopology Command Decision
decider initialState =
  Decider
    { deciderInitialState = initialState,
      decide = \case
        JoinGame name -> decideJoinGame name
        LeaveGame pid -> decideLeaveGame pid
        StartGame -> decideStartGame
        PlaceBet pid amt -> decidePlaceBet pid amt
        DealInitialCards -> decideDealInitialCards
        TakeInsurance pid chips -> decideTakeInsurance pid chips
        RejectInsurance pid -> decideRejectInsurance pid
        ResolveInsurance -> decideResolveInsurance
        Hit pid -> decideHit pid
        Stand pid -> decideStand pid
        DoubleDown pid -> decideDoubleDown pid
        Split pid -> decideSplit pid
        Surrender pid -> decideSurrender pid
        DealerPlay -> decideDealerPlay
        ResolveRound -> decideResolveRound
        RestartGame -> decideRestartGame
        ExitGame -> const (Right GameExited),
      evolve = \game -> \case
        Left _ -> EvolutionResult game
        Right event ->
          let step = case state game of
                LobbyState {} -> evolveLobby
                BettingState {} -> evolveBetting
                DealingState {} -> evolveDealing
                OfferingInsuranceState {} -> evolveOfferingInsurance
                ResolvingInsuranceState {} -> evolveResolvingInsurance
                OpeningTurnState {} -> evolveOpeningTurn
                PlayerTurnState {} -> evolvePlayerTurn
                DealerTurnState {} -> evolveDealerTurn
                ResolvingState {} -> evolveResolution
                ResultState {} -> evolveResult
                ExitedState {} -> const . EvolutionResult
           in step game event
    }
