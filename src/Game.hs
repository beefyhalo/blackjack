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
import Data.Foldable (toList)
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.Zipper qualified as Z
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Domain
import System.Random (StdGen, split)
import "singletons-base" Data.Singletons.Base.TH hiding (Decision)

$( singletons
     [d|
       data GameVertex
         = InLobby
         | AwaitingBets
         | DealingCards
         | OfferingInsurance
         | ResolvingInsurance
         | OpeningTurn
         | PlayerTurn
         | DealerTurn
         | ResolvingHands
         | Result
         | GameOver
         deriving stock (Eq, Show, Enum, Bounded)

       gameTopology :: Topology GameVertex
       gameTopology =
         Topology
           [ (InLobby, [AwaitingBets]),
             (AwaitingBets, [DealingCards]),
             (DealingCards, [OfferingInsurance, OpeningTurn]),
             (OfferingInsurance, [ResolvingInsurance]),
             (ResolvingInsurance, [OpeningTurn, ResolvingHands]),
             (OpeningTurn, [PlayerTurn, DealerTurn, ResolvingHands]),
             (PlayerTurn, [DealerTurn, ResolvingHands]),
             (DealerTurn, [ResolvingHands]),
             (ResolvingHands, [Result]),
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
  LobbyState :: Map.Map PlayerId PlayerSeat -> GameState 'InLobby
  BiddingState :: Map.Map PlayerId PlayerSeat -> GameState 'AwaitingBets
  DealingState :: Map.Map PlayerId PlayerSeat -> Deck -> GameState 'DealingCards
  OfferingInsuranceState :: Deck -> Map.Map PlayerId Player -> Dealer -> GameState 'OfferingInsurance
  ResolvingInsuranceState :: Deck -> Map.Map PlayerId Player -> Dealer -> GameState 'ResolvingInsurance
  OpeningTurnState :: Deck -> Map.Map PlayerId Player -> Dealer -> Set.Set PlayerId -> GameState 'OpeningTurn
  PlayerTurnState :: Deck -> Map.Map PlayerId Player -> Dealer -> GameState 'PlayerTurn
  DealerTurnState :: Deck -> Map.Map PlayerId Player -> Dealer -> GameState 'DealerTurn
  ResolvingState :: Map.Map PlayerId Player -> Dealer -> GameState 'ResolvingHands
  ResultState :: Map.Map PlayerId PlayerSeat -> GameState 'Result
  ExitedState :: GameState 'GameOver

baseMachine :: StdGen -> BaseMachine GameTopology Command (Either GameError Event)
baseMachine stdGen = deciderMachine (decider (InitialState (Game stdGen (LobbyState mempty))))

decider :: InitialState Game -> Decider GameTopology Command (Either GameError Event)
decider initialState =
  Decider
    { deciderInitialState = initialState,
      decide = \case
        JoinGame name -> decideJoinGame name
        LeaveGame pid -> decideLeaveGame pid
        StartGame -> decideStartGame
        PlaceBet pid amt -> decidePlaceBet pid amt
        DealInitialCards -> decideDealInitialCards
        TakeInsurance pid chips -> decideTakeInsurance pid chips
        RejectInsurance pid -> decideRejectInsurance pid
        ResolveInsurance -> decideResolveInsurance
        Hit pid -> decideHit pid
        Stand pid -> decideStand pid
        DoubleDown pid -> decideDoubleDown pid
        Split pid -> decideSplit pid
        Surrender pid -> decideSurrender pid
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
                OfferingInsuranceState {} -> evolveOfferingInsurance
                ResolvingInsuranceState {} -> evolveResolvingInsurance
                OpeningTurnState {} -> evolveOpeningTurn
                PlayerTurnState {} -> evolvePlayerTurn
                DealerTurnState {} -> evolveDealerTurn
                ResolvingState {} -> evolveResolution
                ResultState {} -> evolveResult
                ExitedState {} -> const . EvolutionResult
           in step game event
    }

type Decision = Either GameError Event

decideJoinGame :: Text -> Game vertex -> Decision
decideJoinGame name = \case
  Game {state = LobbyState seats} ->
    let pid = PlayerId (length seats)
     in Right (PlayerJoined pid name)
  _ -> Left GameAlreadyStarted

decideLeaveGame :: PlayerId -> Game vertex -> Decision
decideLeaveGame pid = \case
  Game {state = LobbyState seats}
    | Map.member pid seats -> Right (PlayerLeft pid)
    | otherwise -> Left PlayerNotFound
  _ -> Left GameAlreadyStarted

decideStartGame :: Game vertex -> Decision
decideStartGame = \case
  Game {state = LobbyState seats}
    | null seats -> Left TooFewPlayers
    | otherwise -> Right GameStarted
  _ -> Left GameAlreadyStarted

decidePlaceBet :: PlayerId -> Chips -> Game vertex -> Decision
decidePlaceBet pid amt = \case
  Game {state = BiddingState bets}
    | not (Map.member pid bets) -> Left PlayerNotFound
    | otherwise -> case bets Map.! pid of
        PlayerSeat {stack = PlayerStack bet chips}
          | bet > 0 -> Left PlayerAlreadyBet
          | 0 > bet || bet > chips -> Left MalsizedBet
          | otherwise -> Right (BetPlaced pid amt)
  _ -> Left BadCommand

decideDealInitialCards :: Game vertex -> Decision
decideDealInitialCards = \case
  Game {state = DealingState pids deck} ->
    maybe (Left EmptyDeck) Right do
      (playerHands, deck') <- dealNTo 2 (Map.keys pids) deck
      (dealerHand, _) <- dealN 2 deck'
      Just (CardsDealt playerHands (Dealer dealerHand))
  Game {state = BiddingState {}} -> Left PlayersStillBetting
  _ -> Left BadCommand

decideTakeInsurance :: PlayerId -> Chips -> Game vertex -> Decision
decideTakeInsurance pid insuranceChips = \case
  Game {state = OfferingInsuranceState _ players _}
    | not (Map.member pid players) -> Left PlayerNotFound
    | isJust (insurance (players Map.! pid)) -> Left PlayerAlreadyInsured
    | let Player {playerSeat = PlayerSeat {stack}} = players Map.! pid,
      0 > insuranceChips || insuranceChips < chips stack ->
        Left MalsizedBet
    | otherwise -> Right (PlayerTookInsurance pid insuranceChips)
  _ -> Left BadCommand

decideResolveInsurance :: Game vertex -> Decision
decideResolveInsurance = \case
  Game {state = ResolvingInsuranceState _ players (Dealer dealerHand)} ->
    let isDealerBlackjack = isBlackjack dealerHand
        results = fmap (resolveInsuranceForPlayer isDealerBlackjack) players
     in Right (InsuranceResolved results)
  _ -> Left BadCommand
  where
    resolveInsuranceForPlayer :: Bool -> Player -> InsuranceResult
    resolveInsuranceForPlayer hasBJ Player {insurance} = case insurance of
      Just (TookInsurance amt)
        | hasBJ -> WonInsurancePayout (amt * 2) -- 2:1 insurance payout if the dealer has a blackjack
        | otherwise -> LostInsuranceBet amt
      _ -> NoInsurance

decideRejectInsurance :: PlayerId -> Game vertex -> Decision
decideRejectInsurance pid = \case
  Game {state = OfferingInsuranceState _ players _}
    | not (Map.member pid players) -> Left PlayerNotFound
    | isJust (insurance (players Map.! pid)) -> Left PlayerAlreadyInsured
    | otherwise -> Right (PlayerDeclinedInsurance pid)
  _ -> Left BadCommand

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
      let bet' = bet (Z.current hands),
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

decideDealerPlay :: Game vertex -> Decision
decideDealerPlay = \case
  Game {state = DealerTurnState deck _ dealer} ->
    let dealer' = dealerTurn dealer deck
     in Right (DealerPlayed dealer')
  Game {state = PlayerTurnState {}} -> Left PlayersStillPlaying
  _ -> Left BadCommand
  where
    -- Draw cards until the hand has at least 17 points
    dealerTurn :: Dealer -> Deck -> Dealer
    dealerTurn dealer@(Dealer hand) deck
      | score hand >= 17 = dealer
      | otherwise = case drawCard deck of
          Nothing -> dealer
          Just (card, newDeck) -> dealerTurn (Dealer (addCard card hand)) newDeck

decideResolveRound :: Game vertex -> Decision
decideResolveRound = \case
  Game {state = ResolvingState players dealer} ->
    let dealerOutcome = determineDealerOutcome dealer
        result = fmap (resolvePlayer dealerOutcome) players
     in Right (RoundResolved dealerOutcome result)
  _ -> Left BadCommand
  where
    determineDealerOutcome :: Dealer -> DealerOutcome
    determineDealerOutcome (Dealer hand)
      | isBlackjack hand = DealerBlackjack
      | isBust hand = DealerBust
      | otherwise = DealerFinalScore (score hand)

    resolvePlayer :: DealerOutcome -> Player -> ResolvedResult
    resolvePlayer dealerOutcome Player {hands, playerSeat = PlayerSeat {stack = PlayerStack {chips}}, insurance, hasSurrendered} =
      let (outcomes, totalDelta, totalPush) = unzip3 $ map resolveHand (toList hands)
          net = sum totalDelta
          pushed = sum totalPush
       in ResolvedResult
            { handResults = NE.fromList outcomes,
              nextRoundChips = chips + net,
              nextRoundBet = pushed,
              insuranceResult = undefined
            }
      where
        resolveHand :: HandState -> (Outcome, Int, Bet)
        resolveHand hand@HandState {bet} =
          let outcome = determineOutcome hand dealerOutcome
              delta = chipsDelta bet outcome
              pushAmount = if outcome == Push then bet else 0
           in (outcome, delta, pushAmount)

        determineOutcome :: HandState -> DealerOutcome -> Outcome
        determineOutcome HandState {hand} = \case
          DealerBlackjack
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

        chipsDelta :: Bet -> Outcome -> Chips
        chipsDelta Bet {current} = \case
          PlayerWins Blackjack -> payout 1.5
          PlayerWins _ -> payout 1.0
          DealerWins Surrendered -> -(current `div` 2) -- Player loses half their bet when surrendering
          DealerWins _ -> -current
          Push -> 0
          where
            payout m = floor @Float (fromIntegral current * m)

decideRestartGame :: Game vertex -> Decision
decideRestartGame = \case
  Game {state = ResultState {}} -> Right GameRestarted
  _ -> Left GameAlreadyStarted

decideExitGame :: Game vertex -> Decision
decideExitGame = const (Right GameExited)

evolveLobby :: Game InLobby -> Event -> EvolutionResult GameTopology Game InLobby output
evolveLobby game@Game {state = LobbyState seats} = \case
  PlayerJoined pid name ->
    let seats' = Map.insert pid (newPlayerSeat pid name) seats
     in EvolutionResult game {state = LobbyState seats'}
  PlayerLeft pid ->
    let seats' = Map.delete pid seats
     in EvolutionResult game {state = LobbyState seats'}
  GameStarted -> EvolutionResult game {state = BiddingState seats}
  _ -> EvolutionResult game

evolveBidding :: Game AwaitingBets -> Event -> EvolutionResult GameTopology Game AwaitingBets output
evolveBidding game@Game {stdGen, state = BiddingState seats} = \case
  BetPlaced pid chips ->
    let seats' = Map.adjust (\m -> m {stack = (stack m) {stackBet = Bet chips}}) pid seats
        allBetsAreIn = all ((> 0) . current . stackBet . stack) seats'
        deck = mkDeck stdGen
     in if allBetsAreIn
          then EvolutionResult game {state = DealingState seats' deck}
          else EvolutionResult game {state = BiddingState seats'}
  _ -> EvolutionResult game

evolveDealing :: Game DealingCards -> Event -> EvolutionResult GameTopology Game DealingCards output
evolveDealing game@Game {state = DealingState seats deck} = \case
  CardsDealt playerHands dealer@(Dealer dealerHand)
    | isAce (visibleCard dealer) ->
        let players = fmap (initPlayer emptyHand) seats
         in EvolutionResult game {state = OfferingInsuranceState deck' players dealer}
    | otherwise ->
        let players = Map.fromList [(pid, initPlayer hand (seats Map.! pid)) | (pid, hand) <- playerHands]
         in EvolutionResult game {state = OpeningTurnState deck' players dealer Set.empty}
    where
      deck' =
        let cardsDrawn = sum (map (handSize . snd) playerHands) + handSize dealerHand
         in deck {drawn = drawn deck + cardsDrawn}
  _ -> EvolutionResult game

evolveOfferingInsurance :: Game OfferingInsurance -> Event -> EvolutionResult GameTopology Game OfferingInsurance output
evolveOfferingInsurance game@Game {state = OfferingInsuranceState deck players dealer} = \case
  PlayerTookInsurance pid chips ->
    let players' = Map.adjust (\p -> p {insurance = Just (TookInsurance chips)}) pid players
     in nextState players'
  PlayerDeclinedInsurance pid ->
    let players' = Map.adjust (\p -> p {insurance = Just DeclinedInsurance}) pid players
     in nextState players'
  _ -> EvolutionResult game
  where
    nextState players'
      | all (isJust . insurance) players' = EvolutionResult game {state = ResolvingInsuranceState deck players' dealer}
      | otherwise = EvolutionResult game {state = OfferingInsuranceState deck players' dealer}

evolveResolvingInsurance :: Game ResolvingInsurance -> Event -> EvolutionResult GameTopology Game ResolvingInsurance output
evolveResolvingInsurance game@Game {state = ResolvingInsuranceState deck players dealer@(Dealer dealerHand)} = \case
  InsuranceResolved results
    | isBlackjack dealerHand ->
        let players' = Map.mapWithKey settleInsurance results
         in if isBlackjack dealerHand
              then EvolutionResult game {state = ResolvingState players' dealer}
              else EvolutionResult game {state = OpeningTurnState deck players' dealer Set.empty}
  _ -> EvolutionResult game
  where
    settleInsurance :: PlayerId -> InsuranceResult -> Player
    settleInsurance pid result =
      let player = players Map.! pid
          PlayerSeat {stack} = playerSeat player
          delta = case result of
            WonInsurancePayout amt -> amt
            LostInsuranceBet amt -> -amt
            NoInsurance -> 0
          playerSeat' = (playerSeat player) {stack = stack {chips = chips stack + delta}}
       in player {playerSeat = playerSeat'}

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

evolveDealerTurn :: Game DealerTurn -> Event -> EvolutionResult GameTopology Game DealerTurn output
evolveDealerTurn game@Game {state = DealerTurnState _ players _} = \case
  DealerPlayed dealer -> EvolutionResult game {state = ResolvingState players dealer}
  _ -> EvolutionResult game

evolveResolution :: Game ResolvingHands -> Event -> EvolutionResult GameTopology Game ResolvingHands output
evolveResolution game@Game {state = ResolvingState players _} = \case
  RoundResolved _ outcomes ->
    let settleSeat Player {playerSeat} (ResolvedResult _ bet chips _) = playerSeat {stack = PlayerStack bet chips}
        seats = Map.mapWithKey (settleSeat . (players Map.!)) outcomes
     in EvolutionResult game {state = ResultState seats}
  _ -> EvolutionResult game

evolveResult :: Game Result -> Event -> EvolutionResult GameTopology Game Result output
evolveResult game@Game {state = ResultState seats} = \case
  GameRestarted -> EvolutionResult (withUpdatedRng game {state = LobbyState seats})
  GameExited -> EvolutionResult game {state = ExitedState}
  _ -> EvolutionResult game
