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
import Control.Monad (void)
import Control.Monad.Catch (throwM)
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
import Hedgehog.Range qualified as Range
import System.Random (initStdGen)

main :: IO ()
main = void tests

prop_game_sequential :: Property
prop_game_sequential = property do
  actions <-
    forAll $
      Gen.sequential
        (Range.linear 1 100)
        initialModel
        availableCommands
  stdGen <- initStdGen
  let initialMachine = stateMachine stdGen
  evalStateT
    (executeSequential initialModel actions)
    initialMachine

prop_game_parallel :: Property
prop_game_parallel =
  withRetries 10 $ property do
    actions <-
      forAll $
        Gen.parallel
          (Range.linear 1 100)
          (Range.linear 1 10)
          initialModel
          availableCommands
    stdGen <- initStdGen
    let initialMachine = stateMachine stdGen
    evalStateT
      (executeParallel initialModel actions)
      initialMachine

------------------------------------------------------------------------

initialModel :: Model v
initialModel =
  Model
    { players = Set.empty,
      phase = InLobby
    }

availableCommands :: [Hedgehog.Command Gen TestContext Model]
availableCommands =
  [ joinCommand,
    leaveCommand,
    startGameCommand,
    startGameNotEnoughPlayersCommand,
    gameAlreadyStartedCommand
  ]

data Model v = Model
  { players :: Set.Set (Var PlayerId v),
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
      commandExecute = \cmd -> StateT \machine ->
        case run machine (toDomain cmd) of
          Identity (event, machine') ->
            case matchDecision event of
              Just output -> pure (output, machine')
              Nothing -> throwM (onFail cmd),
      commandCallbacks = callbacks
    }

newtype JoinGameCmd v = JoinGameCmd Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FunctorB, TraversableB)

joinGameCmdGen :: (MonadGen m) => m (JoinGameCmd v)
joinGameCmdGen = JoinGameCmd <$> Gen.text (Range.linear 1 10) Gen.ascii

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
      [ Require \model _ -> phase model == InLobby,
        Update \model _ output -> model {players = Set.insert output (players model)},
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
        Update \model (LeaveGameCmd pid) _ -> model {players = Set.delete pid (players model)},
        Ensure \before after _ pid -> do
          let playerId = Var (Concrete pid)
          assert (Set.member playerId (players before))
          assert (Set.notMember playerId (players after))
          length (players before) - 1 === length (players after)
      ]

-- leaveFailedCommand :: (MonadGen gen) => Hedgehog.Command gen TestContext Model
-- leaveFailedCommand = mkCommand gen toDomain matchDecision onFail callbacks

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
      [ Require \model _ -> phase model == InLobby && not (null (players model)),
        Update \model _ _ -> model {phase = AwaitingBets},
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
      [ Require \model _ -> phase model == InLobby && null (players model),
        Ensure \before after _ _ -> do
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
      [ Require \model _ -> phase model /= InLobby,
        Ensure \before after _ _ -> do
          -- Ensure no changes in players or phase
          phase before === phase after
          players before === players after
      ]

tests :: IO Bool
tests = checkSequential $$(discover)