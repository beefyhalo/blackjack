{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game.PlayerTurn where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.List.NonEmpty.Zipper qualified as Z
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Domain
import GameTopology

decideHit :: PlayerId -> Game vertex -> Decision
decideHit pid = \case
  Game {state = PlayerTurnState deck players _}
    | not (Map.member pid players) -> Left PlayerNotFound
    | otherwise -> case drawCard deck of
        Just (card, _) -> Right (HitCard pid card)
        Nothing -> Left EmptyDeck
  _ -> Left BadCommand

decideStand :: PlayerId -> Game vertex -> Decision
decideStand pid = \case
  Game {state = PlayerTurnState _ players _}
    | not (Map.member pid players) -> Left PlayerNotFound
    | otherwise -> Right (PlayerStood pid)
  _ -> Left BadCommand

decideDoubleDown :: PlayerId -> Game vertex -> Decision
decideDoubleDown pid = \case
  Game {state = OpeningTurnState deck players _ _}
    | not (Map.member pid players) -> Left PlayerNotFound
    | let Player {hands, playerSeat = PlayerSeat {stack = PlayerStack {chips}}} = players Map.! pid,
      let Bet bet' = bet (Z.current hands),
      bet' * 2 > chips ->
        Left MalsizedBet
    | otherwise -> case drawCard deck of
        Just (card, _) -> Right (PlayerDoubledDown pid card)
        Nothing -> Left EmptyDeck
  _ -> Left BadCommand

decideSurrender :: PlayerId -> Game vertex -> Decision
decideSurrender pid = \case
  Game {state = OpeningTurnState _ players _ readyPlayers}
    | not (Map.member pid players) -> Left PlayerNotFound
    | Set.member pid readyPlayers -> Left BadCommand
    | otherwise -> Right (PlayerSurrendered pid)
  _ -> Left BadCommand

decideSplit :: PlayerId -> Game vertex -> Decision
decideSplit pid = \case
  Game {state = OpeningTurnState deck players _ readyPlayers}
    | not (Map.member pid players) -> Left PlayerNotFound
    | Set.member pid readyPlayers -> Left CantSplitMoreThanOnce
    | otherwise -> case players Map.! pid of
        Player {hands} | let HandState {hand} = Z.current hands -> case extractPair hand of
          Just (c1, c2) -> maybe (Left EmptyDeck) Right do
            (d1, deck') <- drawCard deck
            (d2, _) <- drawCard deck'
            Just (PlayerSplitHand pid c1 c2 d1 d2)
          Nothing -> Left BadCommand
  _ -> Left BadCommand

evolveOpeningTurn :: Game OpeningTurn -> Event -> EvolutionResult GameTopology Game OpeningTurn output
evolveOpeningTurn game@Game {state = OpeningTurnState deck players dealer readyPlayers} event = case event of
  PlayerSplitHand pid c1 c2 d1 d2 ->
    let deck' = deck {drawn = drawn deck + 2}
        players' = Map.adjust (splitPlayerHand c1 c2 d1 d2) pid players
        readyPlayers' = Set.insert pid readyPlayers
     in EvolutionResult game {state = OpeningTurnState deck' players' dealer readyPlayers'}
  HitCard pid _ -> advanceState pid
  PlayerStood pid -> advanceState pid
  PlayerDoubledDown pid _ -> advanceState pid
  PlayerSurrendered pid -> advanceState pid
  _ -> EvolutionResult game
  where
    splitPlayerHand :: Card -> Card -> Card -> Card -> Player -> Player
    splitPlayerHand card1 card2 draw1 draw2 player@Player {playerSeat = PlayerSeat {stack = PlayerStack {stackBet}}, hands} =
      let hands' =
            Z.push (initHandState (Hand [card1, draw1])) {bet = stackBet} $
              Z.push (initHandState (Hand [card2, draw2])) {bet = stackBet} hands
       in player {hands = hands'}

    advanceState :: PlayerId -> EvolutionResult GameTopology Game OpeningTurn output
    advanceState pid =
      let intermediate = evolvePlayerTurn game {state = PlayerTurnState deck players dealer} event
       in case intermediate of
            EvolutionResult next@Game {state = DealerTurnState {}} -> EvolutionResult next
            EvolutionResult next@Game {state = PlayerTurnState deck' players' dealer'}
              | Set.size readyPlayers == Map.size players' -> EvolutionResult next
              | otherwise ->
                  let readyPlayers' = Set.insert pid readyPlayers
                   in EvolutionResult next {state = OpeningTurnState deck' players' dealer' readyPlayers'}
            EvolutionResult next@Game {state = ResolvingState {}} -> EvolutionResult next
            _ -> EvolutionResult game

evolvePlayerTurn :: Game PlayerTurn -> Event -> EvolutionResult GameTopology Game PlayerTurn output
evolvePlayerTurn game@Game {state = PlayerTurnState deck players dealer} = \case
  HitCard pid card ->
    let adjust p@Player {hands} =
          let handState = (Z.current hands) {hand = addCard card (hand handState)}
           in p {hands = Z.replace handState hands}
     in nextState nextDeck pid adjust
  PlayerStood pid ->
    let adjust p@Player {hands} =
          let handState = (Z.current hands) {hasStood = True}
           in p {hands = Z.replace handState hands}
     in nextState deck pid adjust
  PlayerDoubledDown pid card ->
    let adjust p@Player {hands} =
          let handState@HandState {hand, bet} = Z.current hands
              handState' = handState {hand = addCard card hand, bet = bet * 2, hasDoubledDown = True}
           in p {hands = Z.replace handState' hands}
     in nextState nextDeck pid adjust
  PlayerSurrendered pid ->
    let adjust p = p {hasSurrendered = True}
     in nextState deck pid adjust
  _ -> EvolutionResult game
  where
    nextDeck = deck {drawn = drawn deck + 1}
    nextState deck' pid adjustPlayer =
      let moveHandFocus p@Player {hands = h} = p {hands = fromMaybe (Z.start h) (Z.right h)}
          players' = Map.adjust (moveHandFocus . adjustPlayer) pid players
       in if all hasCompletedTurn players'
            then
              if any (any hasStood . hands) players'
                then EvolutionResult game {state = DealerTurnState deck' players' dealer}
                else EvolutionResult game {state = ResolvingState players' dealer}
            else EvolutionResult game {state = PlayerTurnState deck' players' dealer}
