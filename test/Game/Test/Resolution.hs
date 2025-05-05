{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Test.Resolution (tests) where

import Crem.Decider (EvolutionResult (..))
import Data.Foldable (for_, toList)
import Data.Map.Strict qualified as Map
import Game.Gen
import Game.Resolution (decideResolution, evolveResolution)
import GameTopology (Game (Game, state), GameState (ResolvingState, ResultState), ResolutionContext (..), SomeGame (SomeGame))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Types
import Prelude hiding (round)

tests :: IO Bool
tests = checkParallel $$discover

prop_decide_emits_RoundResolved :: Property
prop_decide_emits_RoundResolved = property do
  game@Game {state = ResolvingState ctx} <- forAll genResolvingStateGame
  case decideResolution game ResolveRound of
    Right (RoundResolved _ playerSummaries) ->
      length ctx.resolvedRounds === length playerSummaries
    decision -> annotateShow decision >> failure

prop_playerSummary_netChipChange_matches_finalChips :: Property
prop_playerSummary_netChipChange_matches_finalChips = property do
  game@Game {state = ResolvingState ctx} <- forAll genResolvingStateGame
  case decideResolution game ResolveRound of
    Right (RoundResolved _ playerSummaries) ->
      for_ (Map.toList playerSummaries) \(pid, summary) -> do
        let PlayerSummary {netChipChange, finalChips, insurancePayout} = summary
            PlayerRound {player = Player {stack}} = ctx.resolvedRounds Map.! pid
            originalChips = chips stack
            expectedFinal = originalChips + netChipChange - maybe 0 insuranceDelta insurancePayout
        finalChips === expectedFinal
    _ -> failure

prop_pushAmount_matches_push_outcomes :: Property
prop_pushAmount_matches_push_outcomes = property do
  game@Game {state = ResolvingState ctx} <- forAll genResolvingStateGame
  case decideResolution game ResolveRound of
    Right (RoundResolved dealerOutcome summaries) ->
      for_ (Map.toList summaries) \(pid, summary) -> do
        let round = ctx.resolvedRounds Map.! pid
            pushedBetSum =
              sum
                [ bet hand
                  | hand <- toList round.hands,
                    determinePlayerOutcome round hand dealerOutcome == Push
                ]
        nextRoundBet summary === pushedBetSum
    _ -> failure

prop_resolution_fails_on_invalid_phase :: Property
prop_resolution_fails_on_invalid_phase = property do
  SomeGame game <- forAllNonResolvingStateGame
  decideResolution game ResolveRound === Left BadCommand

prop_evolveResolution_transitions_to_ResultState :: Property
prop_evolveResolution_transitions_to_ResultState = property do
  game@Game {state = ResolvingState ResolutionContext {resolvedRounds}} <- forAll genResolvingStateGame
  dealerOutcome <- forAll genDealerOutcome
  summaries <- forAll (genPlayerSummaries (Map.keysSet resolvedRounds))
  let event = RoundResolved dealerOutcome summaries
      evolved = evolveResolution game event
  case evolved of
    EvolutionResult Game {state = ResultState players} -> Map.keysSet players === Map.keysSet summaries
    _ -> failure

prop_evolveResolution_sets_correct_player_stack :: Property
prop_evolveResolution_sets_correct_player_stack = property do
  game@Game {state = ResolvingState ResolutionContext {resolvedRounds}} <- forAll genResolvingStateGame
  dealerOutcome <- forAll genDealerOutcome
  summaries <- forAll (genPlayerSummaries (Map.keysSet resolvedRounds))
  let event = RoundResolved dealerOutcome summaries
      evolved = evolveResolution game event
  case evolved of
    EvolutionResult Game {state = ResultState players} ->
      for_ (Map.toList players) \(pid, player) -> do
        let PlayerSummary {nextRoundBet, finalChips} = summaries Map.! pid
            PlayerStack bet chips = stack player
        bet === nextRoundBet
        chips === finalChips
    _ -> failure

forAllNonResolvingStateGame :: PropertyT IO SomeGame
forAllNonResolvingStateGame = do
  forAllWith (\(SomeGame g) -> show g) $
    Gen.filter (\case SomeGame Game {state = ResolvingState {}} -> False; _ -> True) genGame
