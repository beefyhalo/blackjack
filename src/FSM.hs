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
import Control.Monad.Identity (Identity)
import Crem.BaseMachine
import Crem.Decider (Decider (Decider, decide, deciderInitialState, evolve), EvolutionResult (EvolutionResult), deciderMachine)
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.StateMachine (StateMachine, StateMachineT (Basic), run)
import Crem.Topology
import Data.Functor.Identity (runIdentity)
import Data.Map qualified as Map
import System.IO.Error (catchIOError)
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
         | Result
         | GameOver
         deriving stock (Eq, Show, Enum, Bounded)

       gameTopology :: Topology GameVertex
       gameTopology =
         Topology
           [ (InLobby, [AwaitingBets]),
             (AwaitingBets, [DealingCards]),
             (DealingCards, [PlayerTurn, Result]),
             (PlayerTurn, [PlayerTurn, DealerTurn, Resolving]),
             (DealerTurn, [Resolving]),
             (Resolving, [Result]),
             (Result, [InLobby, GameOver])
           ]
       |]
 )

deriving via AllVertices GameVertex instance RenderableVertices GameVertex

newtype PlayerId = PlayerId String deriving (Eq, Ord, Show, Read)

type Chips = Int

data Player = Player
  { playerId :: PlayerId,
    hand :: Hand,
    bet :: Bet,
    hasStood :: Bool
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
  | ResolveRound
  | RestartGame
  | ExitGame
  -- \| PlayerDoubleDown PlayerId
  -- \| PlayerSplit PlayerId
  -- \| PlayerSurrender PlayerId
  deriving (Read)

data Event
  = PlayerJoined PlayerId
  | PlayerLeft PlayerId
  | GameStarted
  | BetPlaced PlayerId Chips
  | CardsDealt [(PlayerId, Hand)] Hand
  | PlayerHitCard PlayerId Card
  | PlayerStood PlayerId
  | DealerPlayed Hand
  | RoundResolved (Map.Map PlayerId Bet) (Map.Map PlayerId Outcome)
  | GameRestarted
  | GameExited
  deriving
    ( -- | PlayerDoubledDown PlayerId Card
      -- | PlayerSplitHand PlayerId Card Card
      -- | PlayerSurrendered PlayerId
      Eq,
      Show
    )

data Game (vertex :: GameVertex) = Game
  { stdGen :: StdGen,
    state :: GameState vertex
  }

updateR :: Game v -> Game v
updateR game = game {stdGen = let (_, g') = split (stdGen game) in g'}

data GameState (vertex :: GameVertex) where
  LobbyState :: Map.Map PlayerId Bet -> GameState 'InLobby
  BiddingState :: Map.Map PlayerId Bet -> GameState 'AwaitingBets
  DealingState :: Map.Map PlayerId Bet -> GameState 'DealingCards
  PlayerTurnState :: Deck -> Map.Map PlayerId Player -> Hand -> GameState 'PlayerTurn
  DealerTurnState :: Deck -> Map.Map PlayerId Player -> Hand -> GameState 'DealerTurn
  ResolvingState :: Map.Map PlayerId Player -> Hand -> GameState 'Resolving
  ResultState :: Map.Map PlayerId Bet -> Map.Map PlayerId Outcome -> GameState 'Result
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

decider :: InitialState Game -> Decider GameTopology Command (Either GameError Event)
decider initialState =
  Decider
    { deciderInitialState = initialState,
      decide = \case
        JoinGame pid -> \case
          Game {state = LobbyState players} | Map.member pid players -> Left PlayerAlreadyJoined
          Game {state = LobbyState {}} -> Right (PlayerJoined pid)
          _ -> Left GameAlreadyStarted
        LeaveGame pid -> \case
          Game {state = LobbyState players} | Map.member pid players -> Right (PlayerLeft pid)
          Game {state = LobbyState {}} -> Left PlayerNotFound
          _ -> Left GameAlreadyStarted
        StartGame -> \case
          Game {state = LobbyState players} | null players -> Left TooFewPlayers
          Game {state = LobbyState {}} -> Right GameStarted
          _ -> Left GameAlreadyStarted
        PlaceBet pid amt -> \case
          Game {state = BiddingState bets} | not (Map.member pid bets) -> Left PlayerNotFound
          Game {state = BiddingState bets} | current (bets Map.! pid) /= 0 -> Left PlayerAlreadyBet
          Game {state = BiddingState bets} | amt > 0 && amt <= chips (bets Map.! pid) -> Right (BetPlaced pid amt)
          Game {state = BiddingState {}} -> Left MalsizedBet
          _ -> Left BadCommand
        DealInitialCards -> \case
          Game {stdGen, state = DealingState pids} ->
            let deck = mkShuffledDeck stdGen
                (playerHands, Deck rest) = dealNTo 2 (Map.keys pids) deck
                (dealerH, _) = splitAt 2 rest
             in Right $ CardsDealt playerHands (Hand dealerH)
          _ -> Left BadCommand
        PlayerHit pid -> \case
          Game {state = PlayerTurnState _ players _} | not (Map.member pid players) -> Left PlayerNotFound
          Game {state = PlayerTurnState deck _ _} ->
            case draw deck of
              Just (card, _) -> Right (PlayerHitCard pid card)
              Nothing -> Left EmptyDeck
          _ -> Left BadCommand
        PlayerStand pid -> \case
          Game {state = PlayerTurnState _ players _} | not (Map.member pid players) -> Left PlayerNotFound
          Game {state = PlayerTurnState {}} -> Right (PlayerStood pid)
          _ -> Left BadCommand
        DealerPlay -> \case
          Game {state = DealerTurnState deck _ dealer} ->
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
        ResolveRound -> \case
          Game {state = ResolvingState players dealer} ->
            let compareHand Player {hand, bet}
                  | isBust hand = Loss (current bet)
                  | isBust dealer = Win (current bet)
                  | otherwise = case compare (score hand) (score dealer) of
                      LT -> Loss (current bet)
                      GT -> Win (current bet)
                      EQ -> Push
                outcomes = fmap compareHand players
                bets = Map.mapWithKey (\pid Player {bet} -> adjustChips (outcomes Map.! pid) bet) players
                adjustChips (Win win) bet = bet {current = 0, chips = chips bet + 2 * win}
                adjustChips (Loss loss) bet = bet {current = 0, chips = chips bet - loss}
                adjustChips Push bet = bet
             in Right (RoundResolved bets outcomes)
          _ -> Left BadCommand
        RestartGame -> \case
          Game {state = ResultState {}} -> Right GameRestarted
          _ -> Left GameAlreadyStarted
        ExitGame -> const (Right GameExited),
      evolve = \game@Game {stdGen, state} event -> case (state, event) of
        (LobbyState players, Right (PlayerJoined pid)) ->
          let players' = Map.insert pid (Bet 0 100) players
           in EvolutionResult game {state = LobbyState players'}
        (LobbyState players, Right (PlayerLeft pid)) ->
          let players' = Map.delete pid players
           in EvolutionResult game {state = LobbyState players'}
        (LobbyState players, Right GameStarted) ->
          EvolutionResult game {state = BiddingState players}
        (BiddingState bets, Right (BetPlaced pid amt)) ->
          let bets' = Map.adjust (\b -> b {current = amt}) pid bets
              allBetsAreIn = all ((> 0) . current) bets'
           in if allBetsAreIn
                then EvolutionResult game {state = DealingState bets'}
                else EvolutionResult game {state = BiddingState bets'}
        (DealingState bets, Right (CardsDealt _ dealer))
          | score dealer == 21 ->
              let outcomes = Loss . current <$> bets
               in EvolutionResult game {state = ResultState bets outcomes}
        (DealingState bets, Right (CardsDealt playerHands dealer)) ->
          let -- Start from full deck
              Deck deck = mkShuffledDeck stdGen
              -- Remove all cards dealt to players and dealer
              dealtCards = length bets * 2 + 2
              deck' = Deck (drop dealtCards deck)
              players = Map.fromList $ fmap (\(pid, h) -> (pid, Player pid h (bets Map.! pid) False)) playerHands
           in EvolutionResult game {state = PlayerTurnState deck' players dealer}
        (PlayerTurnState (Deck (_ : rest)) players dealer, Right (PlayerHitCard pid card)) ->
          let players' = Map.adjust (\p -> p {hand = addCard card (hand p)}) pid players
              deck = Deck rest
              allBustOrStand = all (\p -> isBust (hand p) || hasStood p) players'
           in if
                | all (isBust . hand) players' -> EvolutionResult game {state = ResolvingState players' dealer}
                | allBustOrStand -> EvolutionResult game {state = DealerTurnState deck players dealer}
                | otherwise -> EvolutionResult game {state = PlayerTurnState deck players' dealer}
        (PlayerTurnState deck players dealer, Right (PlayerStood pid)) ->
          let players' = Map.adjust (\p -> p {hasStood = True}) pid players
              allBustOrStand = all (\p -> isBust (hand p) || hasStood p) players'
           in if allBustOrStand
                then EvolutionResult game {state = DealerTurnState deck players dealer}
                else EvolutionResult game {state = PlayerTurnState deck players' dealer}
        (DealerTurnState _ players _, Right (DealerPlayed dealer)) ->
          EvolutionResult game {state = ResolvingState players dealer}
        (ResolvingState {}, Right (RoundResolved bets outcomes)) ->
          EvolutionResult game {state = ResultState bets outcomes}
        (ResultState bets _, Right GameRestarted) ->
          EvolutionResult (updateR game {state = LobbyState bets})
        (ResultState {}, Right GameExited) -> EvolutionResult game {state = ExitedState}
        (_, _) -> EvolutionResult game
    }

baseMachine :: StdGen -> BaseMachine GameTopology Command (Either GameError Event)
baseMachine g = deciderMachine (decider (InitialState (Game g (LobbyState mempty))))

stateMachine :: StdGen -> StateMachine Command (Either GameError Event)
stateMachine g = Basic (baseMachine g)

gameLoop :: StateMachineT Identity Command (Either GameError Event) -> IO ()
gameLoop machine = do
  command <- commandLoop
  let (output, machine') = runIdentity $ run machine command
  print output
  gameLoop machine'

commandLoop :: IO Command
commandLoop = catchIOError readLn $ const do
  putStrLn "Invalid Command."
  commandLoop