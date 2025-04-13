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
  Game {state = BiddingState seats} ->
    case Map.lookup pid seats of
      Nothing -> Left PlayerSeatNotFound
      Just PlayerSeat {stack = PlayerStack bet chips}
        | bet > 0 -> Left PlayerAlreadyBet
        | amt <= 0 || amt > chips -> Left MalsizedBet
        | otherwise -> Right (BetPlaced pid amt)
  _ -> Left BadCommand

evolveBidding :: Game AwaitingBets -> Event -> EvolutionResult GameTopology Game AwaitingBets output
evolveBidding game@Game {stdGen, state = BiddingState seats} = \case
  BetPlaced pid chips ->
    let updateBet seat = seat {stack = (stack seat) {stackBet = Bet chips}}
        seats' = Map.adjust updateBet pid seats
        allBetsIn = all ((> 0) . stackBet . stack) seats'
     in if allBetsIn
          then EvolutionResult game {state = DealingState seats' (mkDeck stdGen)}
          else EvolutionResult game {state = BiddingState seats'}
  _ -> EvolutionResult game
