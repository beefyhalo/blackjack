{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Game.Bidding (decidePlaceBet, evolveBidding) where

import Crem.Decider (EvolutionResult (EvolutionResult))
import Data.Map.Strict qualified as Map
import Domain
import GameTopology

decidePlaceBet :: PlayerId -> Chips -> Game vertex -> Decision
decidePlaceBet pid amt = \case
  Game {state = BiddingState bets}
    | not (Map.member pid bets) -> Left PlayerNotFound
    | otherwise -> case bets Map.! pid of
        PlayerSeat {stack = PlayerStack (Bet bet) chips}
          | bet > 0 -> Left PlayerAlreadyBet
          | 0 > bet || bet > chips -> Left MalsizedBet
          | otherwise -> Right (BetPlaced pid amt)
  _ -> Left BadCommand

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
