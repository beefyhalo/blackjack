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
import Domain hiding (insurance)
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
  Game {state = OpeningTurnState OpeningContext {insuranceContext = InsuranceContext {context = GameContext {deck, rounds}}}} ->
    withPlayerRound pid rounds \PlayerRound {hands, player = Player {stack = PlayerStack {chips}}} ->
      let currentBet = bet (Z.current hands)
       in withValidBet (currentBet * 2) chips \_ -> case drawCard deck of
            Just (card, _) -> Right (PlayerDoubledDown pid card)
            Nothing -> Left EmptyDeck
  _ -> Left BadCommand

decideSurrender :: PlayerId -> Game vertex -> Decision
decideSurrender pid = \case
  Game {state = OpeningTurnState OpeningContext {insuranceContext = InsuranceContext {context = GameContext {rounds}}, readyPlayers}} ->
    withPlayerRound pid rounds \_ -> if Set.member pid readyPlayers then Left BadCommand else Right (PlayerSurrendered pid)
  _ -> Left BadCommand

decideSplit :: PlayerId -> Game vertex -> Decision
decideSplit pid = \case
  Game {state = OpeningTurnState OpeningContext {insuranceContext = InsuranceContext {context = GameContext {deck, rounds}}, readyPlayers}} ->
    withPlayerRound pid rounds \PlayerRound {hands} ->
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
evolveOpeningTurn game@Game {state = OpeningTurnState OpeningContext {insuranceContext = context@InsuranceContext {context = GameContext deck rounds dealer}, readyPlayers}} event = case event of
  PlayerSplitHand pid c1 c2 d1 d2 ->
    let deck' = deck {drawn = drawn deck + 2}
        rounds' = Map.adjust (splitPlayerHand c1 c2 d1 d2) pid rounds
        readyPlayers' = Set.insert pid readyPlayers
        context' = context {context = GameContext deck' rounds' dealer}
     in EvolutionResult game {state = OpeningTurnState (OpeningContext context' readyPlayers')}
  HitCard pid _ -> advanceState pid
  PlayerStood pid -> advanceState pid
  PlayerDoubledDown pid _ -> advanceState pid
  PlayerSurrendered pid -> advanceState pid
  _ -> EvolutionResult game
  where
    splitPlayerHand :: Card -> Card -> Card -> Card -> PlayerRound -> PlayerRound
    splitPlayerHand card1 card2 draw1 draw2 round@PlayerRound {player = Player {stack = PlayerStack {currentBet}}, hands} =
      let hands' =
            Z.push (initHandState (Hand [card1, draw1])) {bet = currentBet} $
              Z.push (initHandState (Hand [card2, draw2])) {bet = currentBet} hands
       in round {hands = hands'}

    advanceState :: PlayerId -> EvolutionResult GameTopology Game OpeningTurn output
    advanceState pid =
      let intermediate = evolvePlayerTurn game {state = PlayerTurnState context} event
       in case intermediate of
            EvolutionResult next@Game {state = DealerTurnState {}} -> EvolutionResult next
            EvolutionResult next@Game {state = PlayerTurnState InsuranceContext {context = GameContext deck' rounds' dealer'}}
              | Set.size readyPlayers == Map.size rounds' -> EvolutionResult next
              | otherwise ->
                  let readyPlayers' = Set.insert pid readyPlayers
                      context' = context {context = GameContext deck' rounds' dealer'}
                   in EvolutionResult next {state = OpeningTurnState (OpeningContext context' readyPlayers')}
            EvolutionResult next@Game {state = ResolvingState {}} -> EvolutionResult next
            _ -> EvolutionResult game

evolvePlayerTurn :: Game PlayerTurn -> Event -> EvolutionResult GameTopology Game PlayerTurn output
evolvePlayerTurn game@Game {state = PlayerTurnState context@InsuranceContext {context = GameContext deck rounds dealer, insurancePayouts}} = \case
  HitCard pid card ->
    let adjust round@PlayerRound {hands} =
          let handState = Z.current hands
              handState' = handState {hand = addCard card (hand handState)}
           in round {hands = Z.replace handState' hands}
     in advanceState nextDeck pid adjust
  PlayerStood pid ->
    let adjust round@PlayerRound {hands} =
          let handState = (Z.current hands) {hasStood = True}
           in round {hands = Z.replace handState hands}
     in advanceState deck pid adjust
  PlayerDoubledDown pid card ->
    let adjust round@PlayerRound {hands} =
          let handState@HandState {hand, bet} = Z.current hands
              handState' = handState {hand = addCard card hand, bet = bet * 2, hasDoubledDown = True}
           in round {hands = Z.replace handState' hands}
     in advanceState nextDeck pid adjust
  PlayerSurrendered pid ->
    let adjust round = round {hasSurrendered = True}
     in advanceState deck pid adjust
  _ -> EvolutionResult game
  where
    nextDeck = deck {drawn = drawn deck + 1}
    advanceState deck' pid adjustRound =
      let moveHandFocus round@PlayerRound {hands = h} = round {hands = fromMaybe (Z.start h) (Z.right h)}
          rounds' = Map.adjust (moveHandFocus . adjustRound) pid rounds
          hasLost PlayerRound {hands, hasSurrendered} = hasSurrendered || all (isBust . hand) hands
       in if
            | all hasLost rounds' -> EvolutionResult game {state = ResolvingState (ResolutionContext rounds' dealer insurancePayouts)}
            | all hasCompletedTurn rounds' -> EvolutionResult game {state = DealerTurnState context {context = GameContext deck' rounds' dealer}}
            | otherwise -> EvolutionResult game {state = PlayerTurnState context {context = GameContext deck' rounds' dealer}}
