-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE ImportQualifiedPost #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# OPTIONS_GHC -Wno-name-shadowing #-}

module Blackjack where

-- import Card
-- import Data.List.NonEmpty (NonEmpty ((:|)))
-- import Data.List.NonEmpty qualified as NE

-- -- | Game phase phantom type
-- data Phase = Start | Betting | Dealing | PlayerTurn | DealerTurn | GameOver

-- -- | A player's hand and current bet
-- data Player = Player
--   { hand :: Hand,
--     bet :: Bet
--   }
--   deriving (Show)

-- data Bet = Bet
--   { current :: Int,
--     chips :: Int
--   }
--   deriving (Show)

-- data Result = Win | Lose | Tie
--   deriving (Show)

-- data Game (p :: Phase) = Game
--   { deck :: Deck,
--     players :: NE.NonEmpty Player,
--     dealer :: Hand
--   }
--   deriving (Show)

-- -- | Phase-specific state
-- data GameState (p :: Phase) where
--   StartState :: Game 'Start -> GameState 'Start
--   BettingState :: Game 'Start -> GameState 'Betting
--   DealState :: GameState 'Betting -> GameState 'Dealing
--   PlayerTurnState :: Game 'Betting -> Int -> GameState 'PlayerTurn -- currentPlayer index
--   DealerTurnState :: Game 'PlayerTurn -> GameState 'DealerTurn
--   GameOverState :: Game 'DealerTurn -> [Result] -> GameState 'GameOver

-- deriving instance Show (GameState p)

-- -- | Existential wrapper for phase-erased operations
-- data SomeGameState where
--   SomeGameState :: GameState p -> SomeGameState

-- deriving instance Show SomeGameState

-- -- | Phase-typed messages
-- data Msg (p :: Phase) where
--   PlaceBet :: Int -> Msg 'Betting
--   Hit :: Msg 'PlayerTurn
--   Stand :: Msg 'PlayerTurn
--   GoDealer :: Msg 'DealerTurn
--   PlayAgain :: Deck -> Msg 'GameOver
--   Quit :: Msg 'GameOver

-- -- | Core update function: type-safe state transitions
-- update :: Msg p -> GameState p -> SomeGameState
-- update msg state = case (msg, state) of
--   -- Transition from Betting to PlayerTurn (after bet is placed)
--   (PlaceBet amt, BettingState g@(Game _ (p :| ps) _))
--     | amt > 0 && amt <= chips (bet p) ->
--         let b' = Bet amt (chips (bet p) - amt)
--             p' = p {bet = b'}
--             g' = g {players = p' :| ps}
--          in SomeGameState $ PlayerTurnState g' 0
--     | otherwise -> SomeGameState $ BettingState g
--   -- PLAYER TURN: Handle hit and stand
--   (Hit, PlayerTurnState g@(Game _ ps _) i) ->
--     let p' = ps NE.!! i
--      in case draw (deck g) of
--           Nothing -> advancePlayer g (i + 1) -- SomeGameState $ GameOverState g
--           Just (c, d') ->
--             let newHand = addCard c (hand p')
--                 p'' = p' {hand = newHand}
--                 ps' = updateAt i p'' ps
--                 g' = g {deck = d', players = ps'}
--              in if isBust newHand
--                   then advancePlayer g' (i + 1)
--                   else SomeGameState $ PlayerTurnState g' {players = ps'} i
--   (Stand, PlayerTurnState g i) -> advancePlayer g (i + 1)
--   -- DEALER TURN: The dealer draws cards until their hand is at least 17 points
--   (GoDealer, DealerTurnState g@(Game d _ dh)) ->
--     let g' = g {dealer = dealerTurn dh d}
--         result = determineResult g'
--      in SomeGameState $ GameOverState g' result
--   -- GAME OVER: Handle replay or quit actions
--   (PlayAgain newDeck, GameOverState g _) ->
--     let ps' = (\p -> p {hand = emptyHand, bet = (bet p) {current = 0}}) <$> players g
--         g' = Game newDeck ps' emptyHand
--      in SomeGameState $ StartState g'
--   (Quit, GameOverState g o) ->
--     SomeGameState $ GameOverState g o
--   where
--     -- Advance to next player's turn or dealer phase
--     advancePlayer :: Game p -> Int -> SomeGameState
--     advancePlayer (Game d ps dh) i
--       | i < length ps =
--           -- If there are more players, transition to the next player's turn
--           SomeGameState $ PlayerTurnState (Game d ps dh) (i + 1)
--       | otherwise =
--           -- If no players remain, it's the dealer's turn
--           SomeGameState $ DealerTurnState (Game d ps dh)

--     -- Helper to update an element at index `i` in a list
--     updateAt :: Int -> a -> NonEmpty a -> NonEmpty a
--     updateAt i x (y :| ys) = NE.fromList $ take i (y : ys) ++ [x] ++ drop (i + 1) ys

--     -- Dealer's turn: draw cards until the hand has at least 17 points
--     dealerTurn :: Hand -> Deck -> Hand
--     dealerTurn hand deck
--       | score hand >= 17 = hand
--       | otherwise = case draw deck of
--           Nothing -> hand
--           Just (card, newDeck) -> dealerTurn (addCard card hand) newDeck

--     -- Determine the result of the game
--     -- Determine the result of the game
--     determineResult :: Game 'DealerTurn -> [Result]
--     determineResult (Game _ players dealer) =
--       NE.toList $ compareHand <$> players
--       where
--         dealerScore = score dealer
--         compareHand player =
--           let playerScore = score (hand player)
--            in case compare playerScore dealerScore of
--                 LT -> Lose
--                 GT -> Win
--                 EQ -> Tie

-- -- | Game loop: repeatedly ask for player input
-- gameLoop :: SomeGameState -> IO ()
-- gameLoop (SomeGameState state) = case state of
--   StartState g -> do
--     putStrLn "Starting new game..."
--     gameLoop $ SomeGameState $ BettingState g
--   BettingState _ -> do
--     putStrLn "Place your bet (enter an integer):"
--     bet <- readLn
--     gameLoop $ update (PlaceBet bet) state
--   PlayerTurnState g i -> do
--     let p = players g NE.!! i
--     putStrLn $ "Player " ++ show (i + 1) ++ "'s turn. Hand: " ++ show (hand p)
--     putStrLn "Choose action: (1) Hit, (2) Stand"
--     action <- getLine
--     case action of
--       "1" -> gameLoop $ update Hit state
--       "2" -> gameLoop $ update Stand state
--       _ -> do
--         putStrLn "Invalid input. Try again."
--         gameLoop (SomeGameState state)
--   DealState g -> do
--     putStrLn "Dealing"
--   DealerTurnState _ -> do
--     putStrLn "Dealer's turn..."
--     gameLoop $ update GoDealer state
--   GameOverState g result -> do
--     putStrLn $ "Game result: " ++ show result
--     putStrLn "Game over. Would you like to play again? (y/n)"
--     choice <- getLine
--     if choice == "y"
--       then gameLoop $ update (PlayAgain (deck g)) state
--       else putStrLn "Thanks for playing!"