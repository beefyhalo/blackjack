{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}

module Game.StateTest.Model (module Game.StateTest.Model) where

import Control.Exception (Exception)
import Control.Monad.Catch (throwM)
import Control.Monad.State.Strict (StateT (StateT))
import Crem.StateMachine (StateMachineT, run)
import Data.Data (Typeable)
import Data.Functor.Identity (Identity (Identity))
import Data.Set qualified as Set
import Domain
import GameTopology (Decision, GamePhase (InLobby))
import Hedgehog

initialModel :: Model v
initialModel =
  Model
    { players = Set.empty,
      playersLeft = Set.empty,
      playersBetted = Set.empty,
      phase = InLobby
    }

data Model v = Model
  { players :: Set.Set (Var PlayerId v),
    playersLeft :: Set.Set (Var PlayerId v),
    playersBetted :: Set.Set (Var PlayerId v),
    phase :: GamePhase
  }
  deriving (Eq, Show)

data StateError = StateError
  { decision :: Decision,
    reason :: StateErrorReason
  }
  deriving (Show, Exception)

data StateErrorReason
  = TryJoinAfterStart
  | StartFailed
  | LeaveFailed
  | UnexpectedCommand
  deriving (Show)

type TestContext = StateT (StateMachineT Identity Domain.Command Decision) (PropertyT IO)

mkCommand ::
  (TraversableB input, Show output, Show (input Symbolic), Typeable output) =>
  -- | Command generator
  (Model Symbolic -> Maybe (gen (input Symbolic))) ->
  -- | Convert to domain command
  (input Concrete -> Domain.Command) ->
  -- | Match decision
  (Decision -> Maybe output) ->
  -- | Reason if match fails
  StateErrorReason ->
  -- | Callbacks
  [Callback input output Model] ->
  Hedgehog.Command gen TestContext Model
mkCommand gen toDomain matchDecision failReason callbacks =
  Hedgehog.Command
    { commandGen = gen,
      commandExecute = \cmd -> StateT \machine -> do
        case run machine (toDomain cmd) of
          Identity (decision, machine') ->
            case matchDecision decision of
              Just output -> pure (output, machine')
              Nothing -> throwM (StateError decision failReason),
      commandCallbacks = callbacks
    }
