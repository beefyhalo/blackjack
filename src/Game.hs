{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game (baseMachine, decider) where

import Crem.BaseMachine (BaseMachine, InitialState (..))
import Crem.Decider (Decider (..), EvolutionResult (EvolutionResult), deciderMachine)
import Domain
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
        LobbyCmd cmd -> fmap LobbyEvt . (`decideLobby` cmd)
        BettingCmd cmd -> fmap BettingEvt . (`decideBetting` cmd)
        DealingCmd cmd -> fmap DealingEvt . (`decideDealing` cmd)
        InsuranceCmd cmd -> fmap InsuranceEvt . (`decideInsurance` cmd)
        PlayerTurnCmd cmd -> fmap PlayerTurnEvt . (`decidePlayerTurn` cmd)
        DealerTurnCmd cmd -> fmap DealerTurnEvt . (`decideDealerPlay` cmd)
        ResolutionCmd cmd -> fmap ResolutionEvt . (`decideResolution` cmd)
        ResultCmd cmd -> fmap ResultEvt . (`decideResult` cmd)
        ExitGame -> \_ -> Right (ResultEvt GameExited),
      evolve = \game -> \case
        Left _ -> EvolutionResult game
        Right event -> case (state game, event) of
          (LobbyState {}, LobbyEvt evt) -> evolveLobby game evt
          (BettingState {}, BettingEvt evt) -> evolveBetting game evt
          (DealingState {}, DealingEvt evt) -> evolveDealing game evt
          (OfferingInsuranceState {}, InsuranceEvt evt) -> evolveOfferingInsurance game evt
          (ResolvingInsuranceState {}, InsuranceEvt evt) -> evolveResolvingInsurance game evt
          (OpeningTurnState {}, PlayerTurnEvt evt) -> evolveOpeningTurn game evt
          (PlayerTurnState {}, PlayerTurnEvt evt) -> evolvePlayerTurn game evt
          (DealerTurnState {}, DealerTurnEvt evt) -> evolveDealerTurn game evt
          (ResolvingState {}, ResolutionEvt evt) -> evolveResolution game evt
          (ResultState {}, ResultEvt evt) -> evolveResult game evt
          _ -> EvolutionResult game
    }
