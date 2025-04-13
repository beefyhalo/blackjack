{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

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

decideHit :: PlayerId -> Game vertex -> Decision
decideHit pid = \case
  Game {state = PlayerTurnState InsuranceContext {context = GameContext {deck, players}}} ->
    withPlayer pid players \_ -> case drawCard deck of
      Just (card, _) -> Right (HitCard pid card)
      Nothing -> Left EmptyDeck
  _ -> Left BadCommand

decideStand :: PlayerId -> Game vertex -> Decision
decideStand pid = \case
  Game {state = PlayerTurnState InsuranceContext {context = GameContext {players}}} ->
    withPlayer pid players \_ -> Right (PlayerStood pid)
  _ -> Left BadCommand

decideDoubleDown :: PlayerId -> Game vertex -> Decision
decideDoubleDown pid = \case
  Game {state = OpeningTurnState OpeningContext {insurance = InsuranceContext {context = GameContext {deck, players}}}} ->
    withPlayer pid players \Player {hands, playerSeat = PlayerSeat {stack = PlayerStack {chips}}} ->
      let Bet bet' = bet (Z.current hands)
       in if bet' * 2 > chips
            then Left MalsizedBet
            else case drawCard deck of
              Just (card, _) -> Right (PlayerDoubledDown pid card)
              Nothing -> Left EmptyDeck
  _ -> Left BadCommand

decideSurrender :: PlayerId -> Game vertex -> Decision
decideSurrender pid = \case
  Game {state = OpeningTurnState OpeningContext {insurance = InsuranceContext {context = GameContext {players}}, readyPlayers}} ->
    withPlayer pid players \_ -> if Set.member pid readyPlayers then Left BadCommand else Right (PlayerSurrendered pid)
  _ -> Left BadCommand

decideSplit :: PlayerId -> Game vertex -> Decision
decideSplit pid = \case
  Game {state = OpeningTurnState OpeningContext {insurance = InsuranceContext {context = GameContext {deck, players}}, readyPlayers}} ->
    withPlayer pid players \_ ->
      if Set.member pid readyPlayers
        then Left CantSplitMoreThanOnce
        else case players Map.! pid of
          Player {hands} | let HandState {hand} = Z.current hands -> case extractPair hand of
            Just (c1, c2) -> maybe (Left EmptyDeck) Right do
              (d1, deck') <- drawCard deck
              (d2, _) <- drawCard deck'
              Just (PlayerSplitHand pid c1 c2 d1 d2)
            Nothing -> Left BadCommand
  _ -> Left BadCommand

evolveOpeningTurn :: Game OpeningTurn -> Event -> EvolutionResult GameTopology Game OpeningTurn output
evolveOpeningTurn game@Game {state = OpeningTurnState OpeningContext {insurance = insurance@InsuranceContext {context = GameContext deck players dealer}, readyPlayers}} event = case event of
  PlayerSplitHand pid c1 c2 d1 d2 ->
    let deck' = deck {drawn = drawn deck + 2}
        players' = Map.adjust (splitPlayerHand c1 c2 d1 d2) pid players
        readyPlayers' = Set.insert pid readyPlayers
        insurance' = insurance {context = GameContext deck' players' dealer}
     in EvolutionResult game {state = OpeningTurnState (OpeningContext insurance' readyPlayers')}
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
      let intermediate = evolvePlayerTurn game {state = PlayerTurnState insurance} event
       in case intermediate of
            EvolutionResult next@Game {state = DealerTurnState {}} -> EvolutionResult next
            EvolutionResult next@Game {state = PlayerTurnState InsuranceContext {context = GameContext deck' players' dealer'}}
              | Set.size readyPlayers == Map.size players' -> EvolutionResult next
              | otherwise ->
                  let readyPlayers' = Set.insert pid readyPlayers
                      insurance' = insurance {context = GameContext deck' players' dealer'}
                   in EvolutionResult next {state = OpeningTurnState (OpeningContext insurance' readyPlayers')}
            EvolutionResult next@Game {state = ResolvingState {}} -> EvolutionResult next
            _ -> EvolutionResult game

evolvePlayerTurn :: Game PlayerTurn -> Event -> EvolutionResult GameTopology Game PlayerTurn output
evolvePlayerTurn game@Game {state = PlayerTurnState insurance@InsuranceContext {context = GameContext deck players dealer, insurancePayouts}} = \case
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
                then EvolutionResult game {state = DealerTurnState insurance {context = GameContext deck' players' dealer}}
                else EvolutionResult game {state = ResolvingState (ResolutionContext players' dealer insurancePayouts)}
            else EvolutionResult game {state = PlayerTurnState insurance {context = GameContext deck' players' dealer}}
