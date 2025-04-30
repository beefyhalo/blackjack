{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Game.PlayerTurn (decidePlayerTurn, evolveOpeningTurn, evolvePlayerTurn) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.List.NonEmpty.Zipper qualified as Z
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import GameTopology
import Types
import Prelude hiding (round)

decidePlayerTurn :: Game phase -> PlayerTurnCommand -> Either GameError PlayerTurnEvent
decidePlayerTurn = flip \case
  Hit pid -> decideHit pid
  Stand pid -> decideStand pid
  DoubleDown pid -> decideDoubleDown pid
  Split pid -> decideSplit pid
  Surrender pid -> decideSurrender pid

decideHit :: PlayerId -> Game phase -> Either GameError PlayerTurnEvent
decideHit pid = \case
  Game {state = OpeningTurnState ctx} -> hit ctx.insuranceContext.context
  Game {state = PlayerTurnState ctx} -> hit ctx.context
  _ -> Left BadCommand
  where
    hit GameContext {deck, rounds} =
      withPlayerRound pid rounds \_ -> case drawCard deck of
        Just (card, _) -> Right (HitCard pid card)
        Nothing -> Left EmptyDeck

decideStand :: PlayerId -> Game phase -> Either GameError PlayerTurnEvent
decideStand pid = \case
  Game {state = OpeningTurnState ctx} -> stand ctx.insuranceContext.context
  Game {state = PlayerTurnState ctx} -> stand ctx.context
  _ -> Left BadCommand
  where
    stand GameContext {rounds} =
      withPlayerRound pid rounds \_ -> Right (PlayerStood pid)

decideDoubleDown :: PlayerId -> Game phase -> Either GameError PlayerTurnEvent
decideDoubleDown pid Game {state = OpeningTurnState ctx}
  | Set.member pid ctx.readyPlayers = Left PlayerAlreadyActed
decideDoubleDown pid Game {state = OpeningTurnState ctx} =
  withPlayerRound pid ctx.insuranceContext.context.rounds \round ->
    let currentHand = Z.current round.hands
     in withValidBet (currentHand.bet * 2) round.player.stack.chips \_ ->
          case drawCard ctx.insuranceContext.context.deck of
            Just (card, _) -> Right (PlayerDoubledDown pid card)
            Nothing -> Left EmptyDeck
decideDoubleDown _ _ = Left BadCommand

decideSurrender :: PlayerId -> Game phase -> Either GameError PlayerTurnEvent
decideSurrender pid Game {state = OpeningTurnState ctx}
  | Set.member pid ctx.readyPlayers = Left PlayerAlreadyActed
decideSurrender pid Game {state = OpeningTurnState ctx} =
  withPlayerRound pid ctx.insuranceContext.context.rounds \_ -> Right (PlayerSurrendered pid)
decideSurrender _ _ = Left BadCommand

-- TODO check there is enough chips in stack or MalsizedBet
decideSplit :: PlayerId -> Game phase -> Either GameError PlayerTurnEvent
decideSplit pid Game {state = OpeningTurnState ctx}
  | Set.member pid ctx.readyPlayers = Left PlayerAlreadyActed
decideSplit pid Game {state = OpeningTurnState ctx} =
  withPlayerRound pid ctx.insuranceContext.context.rounds \round ->
    case extractSplitPair (Z.current round.hands).hand of
      Just (c1, c2) -> maybe (Left EmptyDeck) Right do
        (d1, deck') <- drawCard ctx.insuranceContext.context.deck
        (d2, _) <- drawCard deck'
        Just (PlayerSplitHand pid c1 c2 d1 d2)
      Nothing -> Left BadCommand -- not a valid split
decideSplit _ _ = Left BadCommand

evolveOpeningTurn :: Game OpeningTurn -> PlayerTurnEvent -> EvolutionResult GameTopology Game OpeningTurn output
evolveOpeningTurn game@Game {state = OpeningTurnState ctx} event =
  case event of
    PlayerSplitHand pid c1 c2 d1 d2 ->
      let deck' = deck {drawn = drawn deck + 2}
          rounds' = Map.adjust (splitPlayerHand c1 c2 d1 d2) pid rounds
          readyPlayers = Set.insert pid ctx.readyPlayers
          insuranceContext = ctx.insuranceContext {context = GameContext deck' rounds' dealer}
       in EvolutionResult game {state = OpeningTurnState ctx {insuranceContext, readyPlayers}}
    HitCard pid _ -> advanceState pid
    PlayerStood pid -> advanceState pid
    PlayerDoubledDown pid _ -> advanceState pid
    PlayerSurrendered pid -> advanceState pid
  where
    InsuranceContext {context = GameContext deck rounds dealer} = ctx.insuranceContext

    splitPlayerHand :: Card -> Card -> Card -> Card -> PlayerRound -> PlayerRound
    splitPlayerHand card1 card2 draw1 draw2 round =
      let hands =
            Z.push (initHandState (Hand [card1, draw1])) {bet = round.player.stack.currentBet}
              . Z.push (initHandState (Hand [card2, draw2])) {bet = round.player.stack.currentBet}
              $ round.hands
       in round {hands}

    advanceState :: PlayerId -> EvolutionResult GameTopology Game OpeningTurn output
    advanceState pid =
      let intermediate = evolvePlayerTurn game {state = PlayerTurnState ctx.insuranceContext} event
       in case intermediate of
            EvolutionResult next@Game {state = DealerTurnState {}} -> EvolutionResult next
            EvolutionResult next@Game {state = PlayerTurnState insuranceContext}
              | Set.size ctx.readyPlayers == Map.size insuranceContext.context.rounds -> EvolutionResult next
              | otherwise ->
                  let round = insuranceContext.context.rounds Map.! pid
                      readyPlayers = if hasCompletedTurn round then Set.insert pid ctx.readyPlayers else ctx.readyPlayers
                   in EvolutionResult next {state = OpeningTurnState OpeningContext {insuranceContext, readyPlayers}}
            EvolutionResult next@Game {state = ResolvingState {}} -> EvolutionResult next
            _ -> EvolutionResult game

evolvePlayerTurn :: Game PlayerTurn -> PlayerTurnEvent -> EvolutionResult GameTopology Game PlayerTurn output
evolvePlayerTurn game@Game {state = PlayerTurnState insuranceContext} = \case
  HitCard pid card ->
    let update = modifyCurrentHand \h -> h {hand = addCard card h.hand}
     in advanceState nextDeck pid update
  PlayerStood pid ->
    let update = modifyCurrentHand \h -> h {hasStood = True}
     in advanceState deck pid update
  PlayerDoubledDown pid card ->
    let update = modifyCurrentHand \h -> h {hand = addCard card h.hand, bet = h.bet * 2, hasDoubledDown = True}
     in advanceState nextDeck pid update
  PlayerSurrendered pid ->
    let update round = round {hasSurrendered = True}
     in advanceState deck pid update
  _ -> EvolutionResult game
  where
    InsuranceContext {context = GameContext deck rounds dealer, insurancePayouts} = insuranceContext
    nextDeck = deck {drawn = drawn deck + 1}
    advanceState deck' pid adjustRound =
      let moveHandFocus round =
            if hasCompletedTurn round
              then round {hands = fromMaybe (Z.start round.hands) (Z.right round.hands)}
              else round
          rounds' = Map.adjust (moveHandFocus . adjustRound) pid rounds
          everyoneLost = all hasLost rounds'
          everyoneDone = all hasCompletedTurn rounds'
          insuranceContext' = insuranceContext {context = GameContext deck' rounds' dealer}
          resolutionContext = ResolutionContext rounds' dealer insurancePayouts
       in if
            | everyoneLost -> EvolutionResult game {state = ResolvingState resolutionContext}
            | everyoneDone -> EvolutionResult game {state = DealerTurnState insuranceContext'}
            | otherwise -> EvolutionResult game {state = PlayerTurnState insuranceContext'}
