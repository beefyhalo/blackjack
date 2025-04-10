{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wmissing-deriving-strategies
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wunticked-promoted-constructors
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wunused-type-patterns
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module FSM where

import Card
import Crem.BaseMachine
import Crem.Decider (Decider (Decider, decide, deciderInitialState, evolve), EvolutionResult (EvolutionResult))
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology
import Data.Map qualified as Map
import System.Random (StdGen, split)
import "singletons-base" Data.Singletons.Base.TH

$( singletons
     [d|
       data GameVertex
         = InLobby
         | AwaitingBets
         | DealingCards
         | PlayerTurn
         | DealerTurn
         | Resolving
         | GameOver
         deriving stock (Eq, Show, Enum, Bounded)

       gameTopology :: Topology GameVertex
       gameTopology =
         Topology
           [ (InLobby, [AwaitingBets]),
             (AwaitingBets, [DealingCards]),
             (DealingCards, [PlayerTurn, Resolving]),
             (PlayerTurn, [PlayerTurn, DealerTurn, Resolving]),
             (DealerTurn, [Resolving]),
             (Resolving, [InLobby, GameOver])
           ]
       |]
 )

deriving via AllVertices GameVertex instance RenderableVertices GameVertex

newtype PlayerId = PlayerId String deriving (Eq, Ord, Show)

type Chips = Int

data Player = Player
  { playerId :: PlayerId,
    hand :: Hand,
    bet :: Bet,
    hasStood :: Bool,
    isBusted :: Bool
  }
  deriving (Show)

data Bet = Bet
  { current :: Chips,
    chips :: Chips
  }
  deriving (Eq, Show)

data Outcome
  = Win Chips
  | Loss Chips
  | Push
  deriving
    ( -- | BlackjackWin Chips
      -- | Surrendered Chips
      Eq,
      Show
    )

data Command
  = JoinGame PlayerId
  | LeaveGame PlayerId
  | StartGame
  | PlaceBet PlayerId Chips
  | DealInitialCards
  | PlayerHit PlayerId
  | PlayerStand PlayerId
  | DealerPlay
  | RestartGame
  | ExitGame

-- \| PlayerDoubleDown PlayerId
-- \| PlayerSplit PlayerId
-- \| PlayerSurrender PlayerId

data Event
  = PlayerJoined PlayerId
  | PlayerLeft PlayerId
  | GameStarted
  | BetPlaced PlayerId Chips
  | CardsDealt [(PlayerId, Hand)] Hand
  | PlayerHitCard PlayerId Card
  | PlayerStood PlayerId
  | DealerPlayed Hand
  | GameRestarted
  | GameExited
  deriving
    ( -- | PlayerDoubledDown PlayerId Card
      -- | PlayerSplitHand PlayerId Card Card
      -- | PlayerSurrendered PlayerId
      Eq,
      Show
    )

data GameState (vertex :: GameVertex) where
  LobbyState :: StdGen -> Map.Map PlayerId Bet -> GameState 'InLobby
  BiddingState :: StdGen -> Map.Map PlayerId Bet -> GameState 'AwaitingBets
  DealingState :: StdGen -> Map.Map PlayerId Bet -> GameState 'DealingCards
  PlayerTurnState :: StdGen -> Deck -> Map.Map PlayerId Player -> Hand -> GameState 'PlayerTurn
  DealerTurnState :: StdGen -> Deck -> Map.Map PlayerId Player -> Hand -> GameState 'DealerTurn
  ResultState :: StdGen -> Map.Map PlayerId Bet -> Map.Map PlayerId Outcome -> Hand -> GameState 'Resolving
  ExitedState :: GameState 'GameOver

data GameError
  = PlayerAlreadyJoined
  | GameAlreadyStarted
  | PlayerNotFound
  | TooFewPlayers
  | BadCommand
  | MalsizedBet
  | PlayerAlreadyBet
  | EmptyDeck
  deriving (Eq, Show)

decider :: InitialState GameState -> Decider GameTopology Command (Either GameError Event)
decider initialState =
  Decider
    { deciderInitialState = initialState,
      decide = \case
        JoinGame pid -> \case
          LobbyState _ players | Map.member pid players -> Left PlayerAlreadyJoined
          LobbyState {} -> Right (PlayerJoined pid)
          _ -> Left GameAlreadyStarted
        LeaveGame pid -> \case
          LobbyState _ players | Map.member pid players -> Right (PlayerLeft pid)
          LobbyState {} -> Left PlayerNotFound
          _ -> Left GameAlreadyStarted
        StartGame -> \case
          LobbyState _ players | null players -> Left TooFewPlayers
          LobbyState {} -> Right GameStarted
          _ -> Left GameAlreadyStarted
        PlaceBet pid amt -> \case
          BiddingState _ bets | not (Map.member pid bets) -> Left PlayerNotFound
          BiddingState _ bets | current (bets Map.! pid) /= 0 -> Left PlayerAlreadyBet
          BiddingState _ bets | amt > 0 && amt <= chips (bets Map.! pid) -> Right (BetPlaced pid amt)
          BiddingState {} -> Left MalsizedBet
          _ -> Left BadCommand
        DealInitialCards -> \case
          DealingState g pids ->
            let deck = mkShuffledDeck g
                (playerHands, Deck rest) = dealNTo 2 (Map.keys pids) deck
                (dealerH, _) = splitAt 2 rest
             in Right $ CardsDealt playerHands (Hand dealerH)
          _ -> Left BadCommand
        PlayerHit pid -> \case
          PlayerTurnState _ _ players _ | not (Map.member pid players) -> Left PlayerNotFound
          PlayerTurnState _ deck _ _ ->
            case draw deck of
              Just (card, _) -> Right (PlayerHitCard pid card)
              Nothing -> Left EmptyDeck
          _ -> Left BadCommand
        PlayerStand pid -> \case
          PlayerTurnState _ _ players _ | not (Map.member pid players) -> Left PlayerNotFound
          PlayerTurnState {} -> Right (PlayerStood pid)
          _ -> Left BadCommand
        DealerPlay -> \case
          DealerTurnState _ deck _ dealer ->
            -- Dealer's turn: draw cards until the hand has at least 17 points
            let dealerTurn :: Hand -> Deck -> Hand
                dealerTurn hand deck0
                  | score hand >= 17 = hand
                  | otherwise = case draw deck0 of
                      Nothing -> hand
                      Just (card, newDeck) -> dealerTurn (addCard card hand) newDeck
                dealer' = dealerTurn dealer deck
             in Right (DealerPlayed dealer')
          _ -> Left BadCommand
        RestartGame -> \case
          ResultState {} -> Right GameRestarted
          _ -> Left GameAlreadyStarted
        ExitGame -> const (Right GameExited),
      evolve = curry \case
        (LobbyState g players, Right (PlayerJoined pid)) -> EvolutionResult (LobbyState g $ Map.insert pid (Bet 0 100) players)
        (LobbyState g players, Right (PlayerLeft pid)) -> EvolutionResult (LobbyState g $ Map.delete pid players)
        (LobbyState g players, Right GameStarted) -> EvolutionResult (BiddingState g players)
        (BiddingState g bets, Right (BetPlaced pid amt)) ->
          let bets' = Map.adjust (\b -> b {current = amt, chips = chips b - amt}) pid bets
              allBetsAreIn = all ((> 0) . current) bets
           in if allBetsAreIn
                then EvolutionResult (DealingState g bets')
                else EvolutionResult (BiddingState g bets')
        (DealingState g bets, Right (CardsDealt _ dealer))
          | score dealer == 21 ->
              let outcomes = Loss . current <$> bets
               in EvolutionResult (ResultState g bets outcomes dealer)
        (DealingState g bets, Right (CardsDealt playerHands dealer)) ->
          let -- Start from full deck
              Deck deck = mkShuffledDeck g
              -- Remove all cards dealt to players and dealer
              dealtCards = length bets * 2 + 2
              deck' = Deck (drop dealtCards deck)
              players = Map.fromList $ (\(pid, h) -> (pid, Player pid h (bets Map.! pid) False False)) <$> playerHands
           in EvolutionResult (PlayerTurnState g deck' players dealer)
        (PlayerTurnState g (Deck (_ : rest)) players dealer, Right (PlayerHitCard pid card)) ->
          let players' = Map.adjust (\p -> p {hand = addCard card (hand p), isBusted = score (hand p) > 21}) pid players
              deck = Deck rest
              allBustOrStand = all (\p -> isBusted p || hasStood p) players'
              bets = fmap bet players'
              outcomes = Loss . current <$> bets
           in if
                | all isBusted players' -> EvolutionResult (ResultState g bets outcomes dealer)
                | allBustOrStand -> EvolutionResult (DealerTurnState g deck players dealer)
                | otherwise -> EvolutionResult (PlayerTurnState g deck players' dealer)
        (PlayerTurnState g deck players dealer, Right (PlayerStood pid)) ->
          let players' = Map.adjust (\p -> p {hasStood = True}) pid players
              allBustOrStand = all (\p -> isBusted p || hasStood p) players'
           in if allBustOrStand
                then EvolutionResult (DealerTurnState g deck players dealer)
                else EvolutionResult (PlayerTurnState g deck players' dealer)
        (DealerTurnState g _ players dealer, Right (DealerPlayed dealerHand)) ->
          let outcomes = fmap compareHand players
              bets = fmap bet players
              dealerScore = score dealer
              compareHand Player {isBusted = True, bet} = Loss (current bet)
              compareHand Player {bet} | dealerScore > 21 = Win (current bet)
              compareHand Player {hand, bet} = case compare (score hand) dealerScore of
                LT -> Loss (current bet)
                GT -> Win (current bet)
                EQ -> Push
           in EvolutionResult (ResultState g bets outcomes dealerHand)
        (ResultState g bets outcomes _, Right GameRestarted) ->
          let bets' = Map.mapWithKey (\pid -> adjustChips (outcomes Map.! pid)) bets
              adjustChips (Win win) bet = bet {current = 0, chips = chips bet + 2 * win}
              adjustChips (Loss _) bet = bet {current = 0}
              adjustChips Push bet = bet
              (_, g') = split g
           in EvolutionResult (LobbyState g' bets')
        (ResultState {}, Right GameExited) -> EvolutionResult ExitedState
        (state, _) -> EvolutionResult state
    }