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

data GameStateWithRng (vertex :: GameVertex) = GameStateWithRng
  { rng :: StdGen,
    state :: GameState vertex
  }

updateR :: GameStateWithRng v -> GameStateWithRng v
updateR game = game {rng = let (_, g') = split (rng game) in g'}

data GameState (vertex :: GameVertex) where
  LobbyState :: Map.Map PlayerId Bet -> GameState 'InLobby
  BiddingState :: Map.Map PlayerId Bet -> GameState 'AwaitingBets
  DealingState :: Map.Map PlayerId Bet -> GameState 'DealingCards
  PlayerTurnState :: Deck -> Map.Map PlayerId Player -> Hand -> GameState 'PlayerTurn
  DealerTurnState :: Deck -> Map.Map PlayerId Player -> Hand -> GameState 'DealerTurn
  ResultState :: Map.Map PlayerId Bet -> Map.Map PlayerId Outcome -> Hand -> GameState 'Resolving
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

decider :: InitialState GameStateWithRng -> Decider GameTopology Command (Either GameError Event)
decider initialState =
  Decider
    { deciderInitialState = initialState,
      decide = \case
        JoinGame pid -> \case
          GameStateWithRng {state = LobbyState players} | Map.member pid players -> Left PlayerAlreadyJoined
          GameStateWithRng {state = LobbyState {}} -> Right (PlayerJoined pid)
          _ -> Left GameAlreadyStarted
        LeaveGame pid -> \case
          GameStateWithRng {state = LobbyState players} | Map.member pid players -> Right (PlayerLeft pid)
          GameStateWithRng {state = LobbyState {}} -> Left PlayerNotFound
          _ -> Left GameAlreadyStarted
        StartGame -> \case
          GameStateWithRng {state = LobbyState players} | null players -> Left TooFewPlayers
          GameStateWithRng {state = LobbyState {}} -> Right GameStarted
          _ -> Left GameAlreadyStarted
        PlaceBet pid amt -> \case
          GameStateWithRng {state = BiddingState bets} | not (Map.member pid bets) -> Left PlayerNotFound
          GameStateWithRng {state = BiddingState bets} | current (bets Map.! pid) /= 0 -> Left PlayerAlreadyBet
          GameStateWithRng {state = BiddingState bets} | amt > 0 && amt <= chips (bets Map.! pid) -> Right (BetPlaced pid amt)
          GameStateWithRng {state = BiddingState {}} -> Left MalsizedBet
          _ -> Left BadCommand
        DealInitialCards -> \case
          GameStateWithRng {rng, state = DealingState pids} ->
            let deck = mkShuffledDeck rng
                (playerHands, Deck rest) = dealNTo 2 (Map.keys pids) deck
                (dealerH, _) = splitAt 2 rest
             in Right $ CardsDealt playerHands (Hand dealerH)
          _ -> Left BadCommand
        PlayerHit pid -> \case
          GameStateWithRng {state = PlayerTurnState _ players _} | not (Map.member pid players) -> Left PlayerNotFound
          GameStateWithRng {state = PlayerTurnState deck _ _} ->
            case draw deck of
              Just (card, _) -> Right (PlayerHitCard pid card)
              Nothing -> Left EmptyDeck
          _ -> Left BadCommand
        PlayerStand pid -> \case
          GameStateWithRng {state = PlayerTurnState _ players _} | not (Map.member pid players) -> Left PlayerNotFound
          GameStateWithRng {state = PlayerTurnState {}} -> Right (PlayerStood pid)
          _ -> Left BadCommand
        DealerPlay -> \case
          GameStateWithRng {state = DealerTurnState deck _ dealer} ->
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
          GameStateWithRng {state = ResultState {}} -> Right GameRestarted
          _ -> Left GameAlreadyStarted
        ExitGame -> const (Right GameExited),
      evolve = \game@GameStateWithRng {rng, state} event -> case (state, event) of
        (LobbyState players, Right (PlayerJoined pid)) ->
          let players' = Map.insert pid (Bet 0 100) players
           in EvolutionResult game {state = LobbyState players'}
        (LobbyState players, Right (PlayerLeft pid)) ->
          let players' = Map.delete pid players
           in EvolutionResult game {state = LobbyState players'}
        (LobbyState players, Right GameStarted) ->
          EvolutionResult game {state = BiddingState players}
        (BiddingState bets, Right (BetPlaced pid amt)) ->
          let bets' = Map.adjust (\b -> b {current = amt, chips = chips b - amt}) pid bets
              allBetsAreIn = all ((> 0) . current) bets
           in if allBetsAreIn
                then EvolutionResult game {state = DealingState bets'}
                else EvolutionResult game {state = BiddingState bets'}
        (DealingState bets, Right (CardsDealt _ dealer))
          | score dealer == 21 ->
              let outcomes = Loss . current <$> bets
               in EvolutionResult game {state = ResultState bets outcomes dealer}
        (DealingState bets, Right (CardsDealt playerHands dealer)) ->
          let -- Start from full deck
              Deck deck = mkShuffledDeck rng
              -- Remove all cards dealt to players and dealer
              dealtCards = length bets * 2 + 2
              deck' = Deck (drop dealtCards deck)
              players = Map.fromList $ (\(pid, h) -> (pid, Player pid h (bets Map.! pid) False False)) <$> playerHands
           in EvolutionResult game {state = PlayerTurnState deck' players dealer}
        (PlayerTurnState (Deck (_ : rest)) players dealer, Right (PlayerHitCard pid card)) ->
          let players' = Map.adjust (\p -> p {hand = addCard card (hand p), isBusted = score (hand p) > 21}) pid players
              deck = Deck rest
              allBustOrStand = all (\p -> isBusted p || hasStood p) players'
              bets = fmap bet players'
              outcomes = Loss . current <$> bets
           in if
                | all isBusted players' -> EvolutionResult game {state = ResultState bets outcomes dealer}
                | allBustOrStand -> EvolutionResult game {state = DealerTurnState deck players dealer}
                | otherwise -> EvolutionResult game {state = PlayerTurnState deck players' dealer}
        (PlayerTurnState deck players dealer, Right (PlayerStood pid)) ->
          let players' = Map.adjust (\p -> p {hasStood = True}) pid players
              allBustOrStand = all (\p -> isBusted p || hasStood p) players'
           in if allBustOrStand
                then EvolutionResult game {state = DealerTurnState deck players dealer}
                else EvolutionResult game {state = PlayerTurnState deck players' dealer}
        (DealerTurnState _ players dealer, Right (DealerPlayed dealerHand)) ->
          let outcomes = fmap compareHand players
              bets = fmap bet players
              dealerScore = score dealer
              compareHand Player {isBusted = True, bet} = Loss (current bet)
              compareHand Player {bet} | dealerScore > 21 = Win (current bet)
              compareHand Player {hand, bet} = case compare (score hand) dealerScore of
                LT -> Loss (current bet)
                GT -> Win (current bet)
                EQ -> Push
           in EvolutionResult (game {state = ResultState bets outcomes dealerHand})
        (ResultState bets outcomes _, Right GameRestarted) ->
          let bets' = Map.mapWithKey (\pid -> adjustChips (outcomes Map.! pid)) bets
              adjustChips (Win win) bet = bet {current = 0, chips = chips bet + 2 * win}
              adjustChips (Loss _) bet = bet {current = 0}
              adjustChips Push bet = bet
           in EvolutionResult (updateR game {state = LobbyState bets'})
        (ResultState {}, Right GameExited) -> EvolutionResult game {state = ExitedState}
        (_, _) -> EvolutionResult game
    }