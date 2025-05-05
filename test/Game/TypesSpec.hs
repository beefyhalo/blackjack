{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell,OverloadedStrings,OverloadedRecordDot #-}

module Game.TypesSpec (tests) where

import Game.Gen
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Types

tests :: IO Bool
tests = checkParallel $$discover

-- 2 card hands never exceed value 21
prop_hand_value_never_exceeds_21 :: Property
prop_hand_value_never_exceeds_21 = property do
  hand <- forAll genTwoCardHand
  assert (score hand <= 21)

-- Adding a card keeps value valid (≤ 21 or busts)
prop_adding_card_value_is_valid :: Property
prop_adding_card_value_is_valid = property do
  hand <- forAll genTwoCardHand
  card <- forAll genCard
  let newHand = addCard card hand
  annotate $ "Before: " ++ show (score hand)
  annotate $ "After:  " ++ show (score newHand)
  assert (score newHand < 31) -- 31 is max if Ace logic fails

-- Busted hands stay busted
prop_adding_card_preserves_bust_behavior :: Property
prop_adding_card_preserves_bust_behavior = property do
  hand <- forAll genHand
  card <- forAll genCard
  let newHand = addCard card hand
  if score hand > 21
    then assert (score newHand > 21)
    else success -- only checks the busted case

-- Ace can reduce value
prop_ace_can_reduce_value :: Property
prop_ace_can_reduce_value = property do
  hand <- forAll $ Gen.filter (not . hasAce) genHand
  suit <- forAll genSuit
  let ace = Card Ace suit
      newHand = addCard ace hand
  assert (score hand <= score newHand + 11)

-- Blackjack only possible with 2 cards
prop_blackjack_requires_two_cards :: Property
prop_blackjack_requires_two_cards = property do
  hand <- forAll genHand
  assert (not (isBlackjack hand) || handSize hand == 2)

-- Blackjack hand always has a 10-value and an Ace
prop_blackjack_value_is_21 :: Property
prop_blackjack_value_is_21 = property do
  hand <- forAll genTwoCardHand
  if isBlackjack hand
    then assert (score hand == 21)
    else success

-- You can only split when cards are the same rank
prop_can_split_only_identical_ranks :: Property
prop_can_split_only_identical_ranks = property do
  hand <- forAll genTwoCardHand
  assert (canSplit hand == (handSize hand == 2 && sameRank hand))
  where
    sameRank (Hand (Card r1 _ : Card r2 _ : _)) = r1 == r2
    sameRank _ = False

-- Dealer hits on soft 16 and under
prop_dealer_hits_under_17 :: Property
prop_dealer_hits_under_17 = property do
  dealer@(Dealer hand) <- forAll genDealer
  (score hand < 17) === dealerShouldHit dealer

-- A hand of only Aces is valued correctly (each additional Ace counts as 1)
prop_all_aces_hand_values_correctly :: Property
prop_all_aces_hand_values_correctly = property do
  n <- forAll $ Gen.int (Range.linear 1 10)
  suit <- forAll genSuit
  let hand = Hand $ replicate n (Card Ace suit)
      expected = if n == 1 then 11 else 11 + (n - 1)
  score hand === expected


prop_chipsDelta_grouped :: Property
prop_chipsDelta_grouped = property do
  bet <- forAll $ genBet 1000 

  label "PlayerWins Blackjack"  
  chipsDelta bet (PlayerWins Blackjack) === round (fromIntegral bet.current * 1.5 :: Float)

  label "PlayerWins Normal"
  chipsDelta bet (PlayerWins OutscoredDealer) === bet.current

  label "DealerWins Normal"
  chipsDelta bet (DealerWins OutscoredByDealer) === -bet.current

  label "DealerWins Surrendered"
  chipsDelta bet (DealerWins Surrendered) === -(bet.current `div` 2)

  label "Push"
  chipsDelta bet Push === 0
