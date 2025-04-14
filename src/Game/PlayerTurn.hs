{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Game.PlayerTurn
  ( decideHit,
    decideStand,
    decideDoubleDown,
    decideSurrender,
    decideSplit,
    evolveOpeningTurn,
    evolvePlayerTurn,
  )
where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.List.NonEmpty.Zipper qualified as Z
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Domain
import GameTopology
import Prelude hiding (round)

decideHit :: PlayerId -> Game vertex -> Decision
decideHit pid = \case
  Game {state = PlayerTurnState InsuranceContext {context = GameContext {deck, rounds}}} ->
    withPlayerRound pid rounds \_ -> case drawCard deck of
      Just (card, _) -> Right (HitCard pid card)
      Nothing -> Left EmptyDeck
  _ -> Left BadCommand

decideStand :: PlayerId -> Game vertex -> Decision
decideStand pid = \case
  Game {state = PlayerTurnState InsuranceContext {context = GameContext {rounds}}} ->
    withPlayerRound pid rounds \_ -> Right (PlayerStood pid)
  _ -> Left BadCommand

decideDoubleDown :: PlayerId -> Game vertex -> Decision
decideDoubleDown pid = \case
  Game {state = OpeningTurnState OpeningContext {insuranceContext}} ->
    let InsuranceContext {context = GameContext {deck, rounds}} = insuranceContext
     in withPlayerRound pid rounds \PlayerRound {hands, player = Player {stack = PlayerStack {chips}}} ->
          let currentBet = bet (Z.current hands)
           in withValidBet (currentBet * 2) chips \_ -> case drawCard deck of
                Just (card, _) -> Right (PlayerDoubledDown pid card)
                Nothing -> Left EmptyDeck
  _ -> Left BadCommand

decideSurrender :: PlayerId -> Game vertex -> Decision
decideSurrender pid = \case
  Game {state = OpeningTurnState OpeningContext {insuranceContext, readyPlayers}} ->
    let InsuranceContext {context = GameContext {rounds}} = insuranceContext
     in withPlayerRound pid rounds \_ ->
          if Set.member pid readyPlayers
            then Left BadCommand
            else Right (PlayerSurrendered pid)
  _ -> Left BadCommand

decideSplit :: PlayerId -> Game vertex -> Decision
decideSplit pid = \case
  Game {state = OpeningTurnState OpeningContext {insuranceContext, readyPlayers}} ->
    let InsuranceContext {context = GameContext {deck, rounds}} = insuranceContext
     in withPlayerRound pid rounds \PlayerRound {hands} ->
          let HandState {hand} = Z.current hands
           in if Set.member pid readyPlayers
                then Left PlayerAlreadyActed -- TODO allow configuration
                else case extractPair hand of
                  Just (c1, c2) -> maybe (Left EmptyDeck) Right do
                    (d1, deck') <- drawCard deck
                    (d2, _) <- drawCard deck'
                    Just (PlayerSplitHand pid c1 c2 d1 d2)
                  Nothing -> Left BadCommand -- not a valid split
  _ -> Left BadCommand

evolveOpeningTurn :: Game OpeningTurn -> Event -> EvolutionResult GameTopology Game OpeningTurn output
evolveOpeningTurn game@Game {state = OpeningTurnState OpeningContext {insuranceContext, readyPlayers}} event =
  case event of
    PlayerSplitHand pid c1 c2 d1 d2 ->
      let deck' = deck {drawn = drawn deck + 2}
          rounds' = Map.adjust (splitPlayerHand c1 c2 d1 d2) pid rounds
          readyPlayers' = Set.insert pid readyPlayers
          insuranceContext' = insuranceContext {context = GameContext deck' rounds' dealer}
       in EvolutionResult game {state = OpeningTurnState (OpeningContext insuranceContext' readyPlayers')}
    HitCard pid _ -> advanceState pid
    PlayerStood pid -> advanceState pid
    PlayerDoubledDown pid _ -> advanceState pid
    PlayerSurrendered pid -> advanceState pid
    _ -> EvolutionResult game
  where
    InsuranceContext {context = GameContext deck rounds dealer} = insuranceContext
    splitPlayerHand :: Card -> Card -> Card -> Card -> PlayerRound -> PlayerRound
    splitPlayerHand card1 card2 draw1 draw2 round@PlayerRound {player = Player {stack = PlayerStack {currentBet}}, hands} =
      let hands' =
            Z.push (initHandState (Hand [card1, draw1])) {bet = currentBet} $
              Z.push (initHandState (Hand [card2, draw2])) {bet = currentBet} hands
       in round {hands = hands'}

    advanceState :: PlayerId -> EvolutionResult GameTopology Game OpeningTurn output
    advanceState pid =
      let intermediate = evolvePlayerTurn game {state = PlayerTurnState insuranceContext} event
       in case intermediate of
            EvolutionResult next@Game {state = DealerTurnState {}} -> EvolutionResult next
            EvolutionResult next@Game {state = PlayerTurnState InsuranceContext {context = GameContext deck' rounds' dealer'}}
              | Set.size readyPlayers == Map.size rounds' -> EvolutionResult next
              | otherwise ->
                  let readyPlayers' = Set.insert pid readyPlayers
                      insuranceContext' = insuranceContext {context = GameContext deck' rounds' dealer'}
                   in EvolutionResult next {state = OpeningTurnState (OpeningContext insuranceContext' readyPlayers')}
            EvolutionResult next@Game {state = ResolvingState {}} -> EvolutionResult next
            _ -> EvolutionResult game

evolvePlayerTurn :: Game PlayerTurn -> Event -> EvolutionResult GameTopology Game PlayerTurn output
evolvePlayerTurn game@Game {state = PlayerTurnState insuranceContext} = \case
  HitCard pid card ->
    let update = modifyCurrentHand \h -> h {hand = addCard card (hand h)}
     in advanceState nextDeck pid update
  PlayerStood pid ->
    let update = modifyCurrentHand \h -> h {hasStood = True}
     in advanceState deck pid update
  PlayerDoubledDown pid card ->
    let update = modifyCurrentHand \h -> h {hand = addCard card (hand h), bet = bet h * 2, hasDoubledDown = True}
     in advanceState nextDeck pid update
  PlayerSurrendered pid ->
    let update round = round {hasSurrendered = True}
     in advanceState deck pid update
  _ -> EvolutionResult game
  where
    InsuranceContext {context = GameContext deck rounds dealer, insurancePayouts} = insuranceContext
    nextDeck = deck {drawn = drawn deck + 1}
    advanceState deck' pid adjustRound =
      let moveHandFocus round@PlayerRound {hands = h} = round {hands = fromMaybe (Z.start h) (Z.right h)}
          rounds' = Map.adjust (moveHandFocus . adjustRound) pid rounds
          everyoneLost = all hasLost rounds'
          everyoneDone = all hasCompletedTurn rounds'
          insuranceContext' = insuranceContext {context = GameContext deck' rounds' dealer}
          resolutionContext = ResolutionContext rounds' dealer insurancePayouts
       in if
            | everyoneLost -> EvolutionResult game {state = ResolvingState resolutionContext}
            | everyoneDone -> EvolutionResult game {state = DealerTurnState insuranceContext'}
            | otherwise -> EvolutionResult game {state = PlayerTurnState insuranceContext'}
