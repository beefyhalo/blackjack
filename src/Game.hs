{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Game (baseMachine, decider) where

import Crem.BaseMachine (BaseMachine, InitialState (..))
import Crem.Decider (Decider (..), EvolutionResult (EvolutionResult), deciderMachine)
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology (STopology (STopology), Topology (Topology), TopologySym0)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Domain
import Domain (LossReason (OutscoredByDealer))
import System.Random (StdGen, split)
import "singletons-base" Data.Singletons.Base.TH hiding (Decision)

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
             (DealingCards, [PlayerTurn, Resolving]),
             (PlayerTurn, [DealerTurn, Resolving]),
             (DealerTurn, [Resolving]),
             (Resolving, [Result]),
             (Result, [InLobby, GameOver])
           ]
       |]
 )

deriving via AllVertices GameVertex instance RenderableVertices GameVertex

data Game (vertex :: GameVertex) = Game
  { stdGen :: StdGen,
    state :: GameState vertex
  }

withUpdatedRng :: Game v -> Game v
withUpdatedRng game = game {stdGen = let (_, g') = split (stdGen game) in g'}

data GameState (vertex :: GameVertex) where
  LobbyState :: Map.Map PlayerId Bet -> GameState 'InLobby
  BiddingState :: Map.Map PlayerId Bet -> GameState 'AwaitingBets
  DealingState :: Map.Map PlayerId Bet -> Deck -> GameState 'DealingCards
  PlayerTurnState :: Deck -> Map.Map PlayerId Player -> Hand -> GameState 'PlayerTurn
  DealerTurnState :: Deck -> Map.Map PlayerId Player -> Hand -> GameState 'DealerTurn
  ResolvingState :: Map.Map PlayerId Player -> Hand -> GameState 'Resolving
  ResultState :: Map.Map PlayerId Bet -> GameState 'Result
  ExitedState :: GameState 'GameOver

baseMachine :: StdGen -> BaseMachine GameTopology Command (Either GameError Event)
baseMachine stdGen = deciderMachine (decider (InitialState (Game stdGen (LobbyState mempty))))

decider :: InitialState Game -> Decider GameTopology Command (Either GameError Event)
decider initialState =
  Decider
    { deciderInitialState = initialState,
      decide = \case
        JoinGame pid -> decideJoinGame pid
        LeaveGame pid -> decideLeaveGame pid
        StartGame -> decideStartGame
        PlaceBet pid amt -> decidePlaceBet pid amt
        DealInitialCards -> decideDealInitialCards
        PlayerHit pid -> decidePlayerHit pid
        PlayerStand pid -> decidePlayerStand pid
        PlayerDoubleDown pid -> decidePlayerDoubleDown pid
        DealerPlay -> decideDealerPlay
        ResolveRound -> decideResolveRound
        RestartGame -> decideRestartGame
        ExitGame -> decideExitGame,
      evolve = \game -> \case
        Left _ -> EvolutionResult game
        Right event ->
          let step = case state game of
                LobbyState {} -> evolveLobby
                BiddingState {} -> evolveBidding
                DealingState {} -> evolveDealing
                PlayerTurnState {} -> evolvePlayerTurn
                DealerTurnState {} -> evolveDealerTurn
                ResolvingState {} -> evolveResolution
                ResultState {} -> evolveResult
                ExitedState {} -> const . EvolutionResult
           in step game event
    }

type Decision = Either GameError Event

decideJoinGame :: PlayerId -> Game vertex -> Decision
decideJoinGame pid = \case
  Game {state = LobbyState players}
    | Map.member pid players -> Left PlayerAlreadyJoined
    | otherwise -> Right (PlayerJoined pid)
  _ -> Left GameAlreadyStarted

decideLeaveGame :: PlayerId -> Game vertex -> Decision
decideLeaveGame pid = \case
  Game {state = LobbyState players}
    | Map.member pid players -> Right (PlayerLeft pid)
    | otherwise -> Left PlayerNotFound
  _ -> Left GameAlreadyStarted

decideStartGame :: Game vertex -> Decision
decideStartGame = \case
  Game {state = LobbyState players}
    | null players -> Left TooFewPlayers
    | otherwise -> Right GameStarted
  _ -> Left GameAlreadyStarted

decidePlaceBet :: PlayerId -> Chips -> Game vertex -> Decision
decidePlaceBet pid amt = \case
  Game {state = BiddingState bets}
    | not (Map.member pid bets) -> Left PlayerNotFound
    | current (bets Map.! pid) /= 0 -> Left PlayerAlreadyBet
    | amt > 0 && amt <= chips (bets Map.! pid) -> Right (BetPlaced pid amt)
    | otherwise -> Left MalsizedBet
  _ -> Left BadCommand

decideDealInitialCards :: Game vertex -> Decision
decideDealInitialCards = \case
  Game {state = DealingState pids deck} ->
    fromMaybe (Left EmptyDeck) do
      (playerHands, deck') <- dealNTo 2 (Map.keys pids) deck
      (dealerHand, _) <- dealN 2 deck'
      pure (Right $ CardsDealt playerHands dealerHand)
  Game {state = BiddingState {}} -> Left PlayersStillBetting
  _ -> Left BadCommand

decidePlayerHit :: PlayerId -> Game vertex -> Decision
decidePlayerHit pid = \case
  Game {state = PlayerTurnState deck players _}
    | not (Map.member pid players) -> Left PlayerNotFound
    | otherwise -> case drawCard deck of
        Just (card, _) -> Right (PlayerHitCard pid card)
        Nothing -> Left EmptyDeck
  _ -> Left BadCommand

decidePlayerStand :: PlayerId -> Game vertex -> Decision
decidePlayerStand pid = \case
  Game {state = PlayerTurnState _ players _}
    | not (Map.member pid players) -> Left PlayerNotFound
    | otherwise -> Right (PlayerStood pid)
  _ -> Left BadCommand

decidePlayerDoubleDown :: PlayerId -> Game vertex -> Decision
decidePlayerDoubleDown pid = \case
  Game {state = PlayerTurnState _ players _}
    | not (Map.member pid players) -> Left PlayerNotFound
    | let Player {hand} = players Map.! pid, handSize hand > 2 -> Left PlayerAlreadyHit
    | let Player {bet} = players Map.! pid, current bet * 2 > chips bet -> Left MalsizedBet
  Game {state = PlayerTurnState deck _ _} -> case drawCard deck of
    Just (card, _) -> Right (PlayerDoubledDown pid card)
    Nothing -> Left EmptyDeck
  _ -> Left BadCommand

decideDealerPlay :: Game vertex -> Decision
decideDealerPlay = \case
  Game {state = DealerTurnState deck _ dealer} ->
    let dealer' = dealerTurn dealer deck
     in Right (DealerPlayed dealer')
  Game {state = PlayerTurnState {}} -> Left PlayersStillPlaying
  _ -> Left BadCommand
  where
    -- Draw cards until the hand has at least 17 points
    dealerTurn :: Hand -> Deck -> Hand
    dealerTurn hand deck
      | score hand >= 17 = hand
      | otherwise = case drawCard deck of
          Nothing -> hand
          Just (card, newDeck) -> dealerTurn (addCard card hand) newDeck

decideResolveRound :: Game vertex -> Decision
decideResolveRound = \case
  Game {state = ResolvingState players dealer} ->
    let dealerOutcome = resolveDealer dealer
        resolvePlayer player =
          let outcome = determineOutcome player dealerOutcome
              (updatedBet, netChips) = applyOutcomeToBet (bet player) outcome
           in ResolvedResult outcome updatedBet netChips
        result = fmap resolvePlayer players
     in Right (RoundResolved dealerOutcome result)
  _ -> Left BadCommand
  where
    resolveDealer :: Hand -> DealerOutcome
    resolveDealer dealer
      | isBlackjack dealer = DealerBlackjack
      | isBust dealer = DealerBust
      | otherwise = DealerFinalScore (score dealer)

    determineOutcome :: Player -> DealerOutcome -> Outcome
    determineOutcome Player {hand, hasInsurance, hasSurrendered} = \case
      DealerBlackjack
        | hasInsurance -> PlayerWins InsurancePayout -- Insurance payout if the dealer has a blackjack
        | isBlackjack hand -> Push
        | otherwise -> DealerWins OutscoredByDealer
      DealerBust
        | hasSurrendered -> DealerWins Surrendered -- Surrender always results in dealer win
        | isBust hand -> DealerWins PlayerBust -- Both busting = dealer wins
        | otherwise -> PlayerWins OutscoredDealer
      DealerFinalScore dealerScore
        | hasSurrendered -> DealerWins Surrendered -- Surrender always results in dealer win
        | isBlackjack hand -> PlayerWins Blackjack
        | isBust hand -> DealerWins PlayerBust
        | otherwise -> case compare (score hand) dealerScore of
            GT -> PlayerWins OutscoredDealer
            LT -> DealerWins OutscoredByDealer
            EQ -> Push
    applyOutcomeToBet :: Bet -> Outcome -> (Bet, Int)
    applyOutcomeToBet bet = \case
      PlayerWins InsurancePayout -> adjustBet (payout 2) -- 2:1 payout for insurance
      PlayerWins Blackjack -> adjustBet (payout 1.5)
      PlayerWins _ -> adjustBet (payout 1.0)
      DealerWins Surrendered -> adjustBet (-(current bet `div` 2)) -- Player loses half their bet when surrendering
      DealerWins _ -> adjustBet (-current bet)
      Push -> (bet {current = 0}, 0)
      where
        payout mult = floor @Float (fromIntegral (current bet) * mult)
        adjustBet netChips = (bet {current = 0, chips = chips bet + netChips}, netChips)

decideRestartGame :: Game vertex -> Decision
decideRestartGame = \case
  Game {state = ResultState {}} -> Right GameRestarted
  _ -> Left GameAlreadyStarted

decideExitGame :: Game vertex -> Decision
decideExitGame = const (Right GameExited)

evolveLobby :: Game InLobby -> Event -> EvolutionResult GameTopology Game InLobby output
evolveLobby game@Game {state = LobbyState players} = \case
  PlayerJoined pid ->
    let players' = Map.insert pid (Bet 0 100) players
     in EvolutionResult game {state = LobbyState players'}
  PlayerLeft pid ->
    let players' = Map.delete pid players
     in EvolutionResult game {state = LobbyState players'}
  GameStarted -> EvolutionResult game {state = BiddingState players}
  _ -> EvolutionResult game

evolveBidding :: Game AwaitingBets -> Event -> EvolutionResult GameTopology Game AwaitingBets output
evolveBidding game@Game {state = BiddingState bets} = \case
  BetPlaced pid chips ->
    let bets' = Map.adjust (\b -> b {current = chips}) pid bets
        allBetsAreIn = all ((> 0) . current) bets'
        deck = mkDeck (stdGen game)
     in if allBetsAreIn
          then EvolutionResult game {state = DealingState bets' deck}
          else EvolutionResult game {state = BiddingState bets'}
  _ -> EvolutionResult game

evolveDealing :: Game DealingCards -> Event -> EvolutionResult GameTopology Game DealingCards output
evolveDealing game@Game {state = DealingState bets deck} = \case
  CardsDealt playerHands dealer
    | isBlackjack dealer ->
        let players = fmap newPlayer bets
         in EvolutionResult game {state = ResolvingState players dealer}
    | otherwise ->
        let players = Map.fromList $ fmap (\(pid, hand) -> (pid, (newPlayer (bets Map.! pid)) {hand = hand})) playerHands
            cardsDrawn = sum (map (handSize . snd) playerHands) + handSize dealer
            deck' = deck {drawn = drawn deck + cardsDrawn}
         in EvolutionResult game {state = PlayerTurnState deck' players dealer}
  _ -> EvolutionResult game

evolvePlayerTurn :: Game PlayerTurn -> Event -> EvolutionResult GameTopology Game PlayerTurn output
evolvePlayerTurn game@Game {state = PlayerTurnState deck players dealer} = \case
  PlayerHitCard pid card ->
    let adjustPlayer p = p {hand = addCard card (hand p)}
     in nextState pid adjustPlayer
  PlayerStood pid ->
    let players' = Map.adjust (\p -> p {hasStood = True}) pid players
        allBustOrStand = all (\p -> isBust (hand p) || hasStood p) players'
     in if allBustOrStand
          then EvolutionResult game {state = DealerTurnState deck players' dealer}
          else EvolutionResult game {state = PlayerTurnState deck players' dealer}
  PlayerDoubledDown pid card ->
    let adjustPlayer p@Player {hand, bet} =
          p {hand = addCard card hand, bet = bet {current = current bet * 2}, hasStood = True}
     in nextState pid adjustPlayer
  _ -> EvolutionResult game
  where
    nextState pid adjustPlayer =
      let deck' = deck {drawn = drawn deck + 1}
          players' = Map.adjust adjustPlayer pid players
          allBustOrStand = all (\p -> isBust (hand p) || hasStood p) players'
       in if
            | all (isBust . hand) players' -> EvolutionResult game {state = ResolvingState players' dealer}
            | allBustOrStand -> EvolutionResult game {state = DealerTurnState deck' players' dealer}
            | otherwise -> EvolutionResult game {state = PlayerTurnState deck' players' dealer}

evolveDealerTurn :: Game DealerTurn -> Event -> EvolutionResult GameTopology Game DealerTurn output
evolveDealerTurn game@Game {state = DealerTurnState _ players _} = \case
  DealerPlayed dealer -> EvolutionResult game {state = ResolvingState players dealer}
  _ -> EvolutionResult game

evolveResolution :: Game Resolving -> Event -> EvolutionResult GameTopology Game Resolving output
evolveResolution game = \case
  RoundResolved _ outcomes -> EvolutionResult game {state = ResultState (fmap nextBet outcomes)}
  _ -> EvolutionResult game

evolveResult :: Game Result -> Event -> EvolutionResult GameTopology Game Result output
evolveResult game@Game {state = ResultState bets} = \case
  GameRestarted -> EvolutionResult (withUpdatedRng game {state = LobbyState bets})
  GameExited -> EvolutionResult game {state = ExitedState}
  _ -> EvolutionResult game
