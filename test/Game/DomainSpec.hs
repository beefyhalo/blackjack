{-# LANGUAGE TemplateHaskell #-}

module Game.DomainSpec (tests) where

import Control.Monad (replicateM)
import Domain
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

tests :: IO Bool
tests = checkSequential $$discover

prop_hand_value_never_exceeds_21 :: Property
prop_hand_value_never_exceeds_21 = property $ do
  hand <- forAll genTwoCardHand
  assert (score hand <= 21)

-- Adding a card keeps value valid (≤ 21 or busts)
prop_adding_card_value_is_valid :: Property
prop_adding_card_value_is_valid = property $ do
  hand <- forAll genTwoCardHand
  card <- forAll genCard
  let newHand = addCard card hand
  annotate $ "Before: " ++ show (score hand)
  annotate $ "After:  " ++ show (score newHand)
  -- if hasAce newHand
  assert (score newHand < 31) -- 31 is max if Ace logic fails

-- Busted hands stay busted
prop_adding_card_preserves_bust_behavior :: Property
prop_adding_card_preserves_bust_behavior = property $ do
  hand <- forAll genHand
  card <- forAll genCard
  let newHand = addCard card hand
  if score hand > 21
    then assert (score newHand > 21)
    else success -- only checks the busted case

-- Ace can reduce value
prop_ace_can_reduce_value :: Property
prop_ace_can_reduce_value = property $ do
  hand <- forAll $ Gen.filter (not . hasAce) genHand
  suit <- forAll genSuit
  let ace = Card Ace suit
      newHand = addCard ace hand
  assert (score hand <= score newHand + 11)

-- Blackjack only possible with 2 cards
prop_blackjack_requires_two_cards :: Property
prop_blackjack_requires_two_cards = property $ do
  hand <- forAll genHand
  assert (not (isBlackjack hand) || handSize hand == 2)

-- Blackjack hand always has a 10-value and an Ace
prop_blackjack_value_is_21 :: Property
prop_blackjack_value_is_21 = property $ do
  hand <- forAll genTwoCardHand
  if isBlackjack hand
    then assert (score hand == 21)
    else success

-- You can only split when cards are the same rank
prop_can_split_only_identical_ranks :: Property
prop_can_split_only_identical_ranks = property $ do
  hand <- forAll genTwoCardHand
  assert (canSplit hand == (handSize hand == 2 && sameRank hand))
  where
    sameRank (Hand [Card r1 _, Card r2 _]) = r1 == r2
    sameRank _ = False

-- Dealer hits on soft 16 and under
prop_dealer_hits_under_17 :: Property
prop_dealer_hits_under_17 = property $ do
  dealer@(Dealer hand) <- forAll genDealer
  (score hand < 17) === dealerShouldHit dealer

-- A hand of only Aces is valued correctly (each additional Ace counts as 1)
prop_all_aces_hand_values_correctly :: Property
prop_all_aces_hand_values_correctly = property $ do
  n <- forAll $ Gen.int (Range.linear 1 10)
  suit <- forAll genSuit
  let hand = Hand $ replicate n (Card Ace suit)
      expected = if n == 1 then 11 else 11 + (n - 1)
  score hand === expected

hasAce :: Hand -> Bool
hasAce (Hand hand) = any isAce hand

genRank :: Gen Rank
genRank = Gen.enumBounded

genSuit :: Gen Suit
genSuit = Gen.enumBounded

genCard :: Gen Card
genCard = liftA2 Card genRank genSuit

genHand :: Gen Hand
genHand = Hand <$> Gen.list (Range.linear 1 5) genCard

genTwoCardHand :: Gen Hand
genTwoCardHand = Hand <$> replicateM 2 genCard

genDealer :: Gen Dealer
genDealer = fmap Dealer genHand

blackjackHand :: Gen Hand
blackjackHand = do
  tenCard <- Gen.element [Ten, Jack, Queen, King]
  suit1 <- Gen.enumBounded
  suit2 <- Gen.enumBounded
  Gen.element
    [ Hand [Card Ace suit1, Card tenCard suit2],
      Hand [Card tenCard suit2, Card Ace suit1]
    ]