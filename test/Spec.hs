{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

import Application (stateMachine)
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.State.Strict (StateT (StateT), evalStateT)
import Crem.StateMachine (StateMachineT, run)
import Data.Data (Typeable)
import Data.Functor.Identity (Identity (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Domain
import GHC.Generics (Generic)
import GameTopology (Decision, GamePhase (AwaitingBets, InLobby))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Main (defaultMain)
import Hedgehog.Range qualified as Range
import System.Random (initStdGen)

main :: IO ()
main = defaultMain [tests]

prop_game_sequential :: Property
prop_game_sequential = property do
  actions <-
    forAll $
      Gen.sequential
        (Range.linear 1 100)
        initialModel
        commands
  stdGen <- initStdGen
  let machine = stateMachine stdGen
  evalStateT
    (executeSequential initialModel actions)
    machine
  where
    commands =
      [ joinCommand,
        leaveCommand,
        leaveGamePlayerNotFoundCommand,
        startGameCommand,
        startGameNotEnoughPlayersCommand,
        gameAlreadyStartedCommand
      ]

------------------------------------------------------------------------

initialModel :: Model v
initialModel =
  Model
    { players = Set.empty,
      leftPlayers = Set.empty,
      phase = InLobby
    }

data Model v = Model
  { players :: Set.Set (Var PlayerId v),
    leftPlayers :: Set.Set (Var PlayerId v),
    phase :: GamePhase
  }
  deriving (Eq, Show)

data StateError
  = TryJoinAfterStart (JoinGameCmd Concrete)
  | StartFailed
  | LeaveFailed
  | UnexpectedCommand (GameAlreadyStartedCmd Concrete)
  deriving (Show, Exception)

type TestContext = StateT (StateMachineT Identity Domain.Command Decision) (PropertyT IO)

mkCommand ::
  (TraversableB input, Show output, Show (input Symbolic), Typeable output) =>
  -- | Command generator
  (Model Symbolic -> Maybe (gen (input Symbolic))) ->
  -- | Convert to domain command
  (input Concrete -> Domain.Command) ->
  -- | Match decision
  (Decision -> Maybe output) ->
  -- | Error if match fails
  (input Concrete -> StateError) ->
  -- | Callbacks
  [Callback input output Model] ->
  Hedgehog.Command gen TestContext Model
mkCommand gen toDomain matchDecision onFail callbacks =
  Hedgehog.Command
    { commandGen = gen,
      commandExecute = \cmd -> StateT \machine -> do
        case run machine (toDomain cmd) of
          Identity (decision, machine') ->
            case matchDecision decision of
              Just output -> pure (output, machine')
              Nothing -> throwM (onFail cmd),
      commandCallbacks = callbacks
    }

newtype JoinGameCmd v = JoinGameCmd Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FunctorB, TraversableB)

joinGameCmdGen :: (MonadGen m) => m (JoinGameCmd v)
joinGameCmdGen = JoinGameCmd <$> Gen.text (Range.linear 1 10) Gen.latin1

joinGameCmdToDomain :: JoinGameCmd Concrete -> Domain.Command
joinGameCmdToDomain (JoinGameCmd name) = JoinGame name

joinCommand :: (MonadGen gen) => Hedgehog.Command gen TestContext Model
joinCommand = mkCommand gen toDomain matchDecision onFail callbacks
  where
    gen model = if phase model == InLobby then Just joinGameCmdGen else Nothing
    toDomain = joinGameCmdToDomain
    matchDecision = \case Right (PlayerJoined pid _) -> Just pid; _ -> Nothing
    onFail = TryJoinAfterStart
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
leaveGamePlayerNotFoundCmdGen = fmap LeaveGameCmd . Gen.element . leftPlayers

leaveGameCmdToDomain :: LeaveGameCmd Concrete -> Domain.Command
leaveGameCmdToDomain (LeaveGameCmd pid) = LeaveGame (concrete pid)

leaveCommand :: (MonadGen gen) => Hedgehog.Command gen TestContext Model
leaveCommand = mkCommand gen toDomain matchDecision onFail callbacks
  where
    gen model = if phase model == InLobby && not (null (players model)) then Just (leaveGameCmdGen model) else Nothing
    toDomain = leaveGameCmdToDomain
    matchDecision = \case Right (PlayerLeft pid) -> Just pid; _ -> Nothing
    onFail _ = LeaveFailed
    callbacks =
      [ Require \model (LeaveGameCmd pid) -> Set.member pid (players model),
        Update \model (LeaveGameCmd pid) _ -> model {players = Set.delete pid (players model), leftPlayers = Set.insert pid (leftPlayers model)},
        Ensure \before after _ pid -> do
          let playerId = Var (Concrete pid)
          assert (Set.member playerId (players before))
          assert (Set.notMember playerId (players after))
          length (players before) - 1 === length (players after)
      ]

leaveGamePlayerNotFoundCommand :: (MonadGen gen) => Hedgehog.Command gen TestContext Model
leaveGamePlayerNotFoundCommand = mkCommand gen toDomain matchDecision onFail callbacks
  where
    gen model = if phase model == InLobby && not (null (leftPlayers model)) then Just (leaveGamePlayerNotFoundCmdGen model) else Nothing
    toDomain = leaveGameCmdToDomain
    matchDecision = \case Left (PlayerNotFound pid) -> Just pid; _ -> Nothing
    onFail _ = LeaveFailed
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
startGameCommand = mkCommand gen toDomain matchDecision onFail callbacks
  where
    gen model = if phase model == InLobby && not (null (players model)) then Just startGameCmdGen else Nothing
    toDomain = startGameCmdToDomain
    matchDecision = \case Right GameStarted -> Just (); _ -> Nothing
    onFail _ = StartFailed
    callbacks =
      [ Update \model _ _ -> model {phase = AwaitingBets},
        Ensure \before after _ _ -> do
          phase before === InLobby
          phase after === AwaitingBets
      ]

startGameNotEnoughPlayersCommand :: (MonadGen gen) => Hedgehog.Command gen TestContext Model
startGameNotEnoughPlayersCommand = mkCommand gen toDomain matchDecision onFail callbacks
  where
    gen model = if phase model == InLobby && null (players model) then Just startGameCmdGen else Nothing
    toDomain = startGameCmdToDomain
    matchDecision = \case Left TooFewPlayers -> Just (); _ -> Nothing
    onFail _ = StartFailed
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
gameAlreadyStartedCommand = mkCommand gen toDomain matchDecision onFail callbacks
  where
    gen model = if phase model /= InLobby then Just (genGameAlreadyStartedCmd model) else Nothing
    toDomain = gameAlreadyStartedToDomain
    matchDecision = \case Left GameAlreadyStarted -> Just (); _ -> Nothing
    onFail = UnexpectedCommand
    callbacks =
      [ Ensure \before after _ _ -> do
          -- Ensure no changes in players or phase
          before === after
      ]

tests :: IO Bool
tests = checkSequential $$(discover)
