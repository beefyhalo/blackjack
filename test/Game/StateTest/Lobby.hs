{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Game.StateTest.Lobby
  ( joinCommand,
    leaveCommand,
    leaveGamePlayerNotFoundCommand,
    startGameCommand,
    startGameNotEnoughPlayersCommand,
    gameAlreadyStartedCommand,
  )
where

import Data.Set qualified as Set
import Data.Text (Text)
import Domain
import GHC.Generics (Generic)
import Game.StateTest.Model
import GameTopology (GamePhase (AwaitingBets, InLobby))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

newtype JoinGameCmd v = JoinGameCmd Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FunctorB, TraversableB)

joinGameCmdGen :: (MonadGen m) => m (JoinGameCmd v)
joinGameCmdGen = JoinGameCmd <$> Gen.text (Range.linear 1 10) Gen.latin1

joinGameCmdToDomain :: JoinGameCmd Concrete -> Domain.Command
joinGameCmdToDomain (JoinGameCmd name) = JoinGame name

joinCommand :: (MonadGen gen) => Hedgehog.Command gen TestContext Model
joinCommand = mkCommand gen toDomain matchDecision failReason callbacks
  where
    gen model = if phase model == InLobby then Just joinGameCmdGen else Nothing
    toDomain = joinGameCmdToDomain
    matchDecision = \case Right (PlayerJoined pid _) -> Just pid; _ -> Nothing
    failReason = TryJoinAfterStart
    callbacks =
      [ Update \model _ output -> model {players = Set.insert output (players model)},
        Ensure \before after _ pid -> do
          let playerId = Var (Concrete pid)
          assert (Set.notMember playerId (players before))
          assert (Set.member playerId (players after))
          length (players before) + 1 === length (players after)
      ]

newtype LeaveGameCmd v = LeaveGameCmd (Var PlayerId v)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FunctorB, TraversableB)

leaveGameCmdGen :: (MonadGen m) => Model v -> m (LeaveGameCmd v)
leaveGameCmdGen = fmap LeaveGameCmd . Gen.element . players

leaveGamePlayerNotFoundCmdGen :: (MonadGen m) => Model v -> m (LeaveGameCmd v)
leaveGamePlayerNotFoundCmdGen = fmap LeaveGameCmd . Gen.element . playersLeft

leaveGameCmdToDomain :: LeaveGameCmd Concrete -> Domain.Command
leaveGameCmdToDomain (LeaveGameCmd pid) = LeaveGame (concrete pid)

leaveCommand :: (MonadGen gen) => Hedgehog.Command gen TestContext Model
leaveCommand = mkCommand gen toDomain matchDecision failReason callbacks
  where
    gen model = if phase model == InLobby && not (null (players model)) then Just (leaveGameCmdGen model) else Nothing
    toDomain = leaveGameCmdToDomain
    matchDecision = \case Right (PlayerLeft pid) -> Just pid; _ -> Nothing
    failReason = LeaveFailed
    callbacks =
      [ Require \model (LeaveGameCmd pid) -> Set.member pid (players model),
        Update \model (LeaveGameCmd pid) _ -> model {players = Set.delete pid (players model), playersLeft = Set.insert pid (playersLeft model)},
        Ensure \before after _ pid -> do
          let playerId = Var (Concrete pid)
          assert (Set.member playerId (players before))
          assert (Set.notMember playerId (players after))
          length (players before) - 1 === length (players after)
      ]

leaveGamePlayerNotFoundCommand :: (MonadGen gen) => Hedgehog.Command gen TestContext Model
leaveGamePlayerNotFoundCommand = mkCommand gen toDomain matchDecision failReason callbacks
  where
    gen model = if phase model == InLobby && not (null (playersLeft model)) then Just (leaveGamePlayerNotFoundCmdGen model) else Nothing
    toDomain = leaveGameCmdToDomain
    matchDecision = \case Left (PlayerNotFound pid) -> Just pid; _ -> Nothing
    failReason = LeaveFailed
    callbacks =
      [ Require \model (LeaveGameCmd pid) -> Set.notMember pid (players model),
        Ensure \before after (LeaveGameCmd pid) _ -> do
          assert (Set.notMember pid (players before))
          assert (Set.notMember pid (players after))
          before === after
      ]

data StartGameCmd v = StartGameCmd
  deriving (Eq, Show, Generic, FunctorB, TraversableB)

startGameCmdGen :: (MonadGen m) => m (StartGameCmd v)
startGameCmdGen = Gen.constant StartGameCmd

startGameCmdToDomain :: StartGameCmd Concrete -> Domain.Command
startGameCmdToDomain _ = StartGame

startGameCommand :: (MonadGen gen) => Hedgehog.Command gen TestContext Model
startGameCommand = mkCommand gen toDomain matchDecision failReason callbacks
  where
    gen model = if phase model == InLobby && not (null (players model)) then Just startGameCmdGen else Nothing
    toDomain = startGameCmdToDomain
    matchDecision = \case Right GameStarted -> Just (); _ -> Nothing
    failReason = StartFailed
    callbacks =
      [ Require \model _ -> not (null (players model)),
        Update \model _ _ -> model {phase = AwaitingBets},
        Ensure \before after _ _ -> do
          phase before === InLobby
          phase after === AwaitingBets
      ]

startGameNotEnoughPlayersCommand :: (MonadGen gen) => Hedgehog.Command gen TestContext Model
startGameNotEnoughPlayersCommand = mkCommand gen toDomain matchDecision failReason callbacks
  where
    gen model = if phase model == InLobby && null (players model) then Just startGameCmdGen else Nothing
    toDomain = startGameCmdToDomain
    matchDecision = \case Left TooFewPlayers -> Just (); _ -> Nothing
    failReason = StartFailed
    callbacks =
      [ Ensure \before after _ _ -> do
          phase before === InLobby
          phase after === InLobby
      ]

data GameAlreadyStartedCmd v
  = GameAlreadyStartedJoinCmd (JoinGameCmd v)
  | GameAlreadyStartedLeaveCmd (LeaveGameCmd v)
  | GameAlreadyStartedStartCmd (StartGameCmd v)
  deriving (Eq, Show, Generic, FunctorB, TraversableB)

genGameAlreadyStartedCmd :: (MonadGen m) => Model v -> m (GameAlreadyStartedCmd v)
genGameAlreadyStartedCmd model =
  Gen.choice
    [ fmap GameAlreadyStartedJoinCmd joinGameCmdGen,
      fmap GameAlreadyStartedLeaveCmd (leaveGameCmdGen model),
      fmap GameAlreadyStartedStartCmd startGameCmdGen
    ]

gameAlreadyStartedToDomain :: GameAlreadyStartedCmd Concrete -> Domain.Command
gameAlreadyStartedToDomain = \case
  GameAlreadyStartedJoinCmd cmd -> joinGameCmdToDomain cmd
  GameAlreadyStartedLeaveCmd cmd -> leaveGameCmdToDomain cmd
  GameAlreadyStartedStartCmd cmd -> startGameCmdToDomain cmd

gameAlreadyStartedCommand :: (MonadGen m) => Hedgehog.Command m TestContext Model
gameAlreadyStartedCommand = mkCommand gen toDomain matchDecision failReason callbacks
  where
    gen model = if phase model /= InLobby then Just (genGameAlreadyStartedCmd model) else Nothing
    toDomain = gameAlreadyStartedToDomain
    matchDecision = \case Left GameAlreadyStarted -> Just (); _ -> Nothing
    failReason = UnexpectedCommand
    callbacks =
      [ Require \model _ -> phase model /= InLobby,
        Ensure \before after _ _ -> do
          -- Ensure no changes in players or phase
          before === after
      ]
