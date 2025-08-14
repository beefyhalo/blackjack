# 🃏 Blackjack: A Compositional State Machine Implementation

[![Haskell](https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white)](https://www.haskell.org/)
[![License: BSD-3](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg?style=for-the-badge)](https://opensource.org/licenses/BSD-3-Clause)

A sophisticated implementation of Blackjack demonstrating advanced functional programming patterns using **CREM** (Compositional Representable Executable Machines) and **Threepenny GUI** with reactive programming principles.

## 🏗️ Architecture Overview

This project showcases a clean, type-safe approach to modeling complex stateful applications through:

- **🎯 CREM State Machines**: Compositional, type-safe state modeling with compile-time guarantees
- **⚛️ Threepenny Reactive Model**: Functional reactive programming for the web UI
- **🔒 Type-Level Safety**: Game phases encoded at the type level preventing invalid transitions
- **🎲 Property-Based Testing**: Comprehensive test coverage using Hedgehog generators

### Core Design Philosophy

The application is built around the principle of **correctness by construction**, where the type system prevents invalid states and transitions, making bugs impossible rather than just unlikely.

## 🚀 Features

### Game Implementation
- **Complete Blackjack Rules**: Hit, Stand, Double Down, Split, Surrender, Insurance
- **Multi-Player Support**: Concurrent player management with individual game states  
- **Realistic Card Mechanics**: Proper deck shuffling, card dealing, and scoring
- **Dealer AI**: Automated dealer play following standard casino rules

### Technical Features
- **Type-Safe State Transitions**: Each game phase is a distinct type
- **Compositional Architecture**: State machines can be combined and extended
- **Reactive UI**: Real-time updates with functional reactive programming
- **Comprehensive Testing**: Property-based tests covering all game scenarios
- **Multiple Interfaces**: Console and web-based gameplay

## 🔧 Design Deep Dive

### Type-Level State Machine Architecture

The project demonstrates **phantom types** and **GADTs** to create a state machine where invalid transitions are impossible at compile time. This approach eliminates entire classes of bugs through the type system.

#### Phase-Indexed Types

Each game phase is encoded as a type-level constant, creating a **phantom type parameter** that carries no runtime information but provides compile-time safety:

```haskell
-- Game phases as type-level constants
data GamePhase
  = InLobby | AwaitingBets | DealingCards 
  | OfferingInsurance | ResolvingInsurance
  | OpeningTurn | PlayerTurn | DealerTurn 
  | ResolvingHands | Result | GameOver

-- The Game type is parameterized by the current phase
data Game (phase :: GamePhase) = Game
  { stdGen :: StdGen
  , nextPlayerId :: Int  
  , state :: GameState phase  -- Phase-specific state
  }
```

This means you can only call phase-specific functions when the game is in the correct phase:

```haskell
-- Only valid when game is in 'PlayerTurn phase
decidePlayerTurn :: Game 'PlayerTurn -> PlayerTurnCommand -> Either GameError PlayerTurnEvent

-- Only valid when game is in 'DealerTurn phase  
decideDealerPlay :: Game 'DealerTurn -> DealerTurnCommand -> Either GameError DealerTurnEvent
```

#### GADT-Based State Constraints

**Generalized Algebraic Data Types (GADTs)** enforce that each phase can only contain appropriate state data:

```haskell
data GameState (phase :: GamePhase) where
  LobbyState :: PlayerMap → GameState 'InLobby
  BettingState :: PlayerMap → GameState 'AwaitingBets  
  DealingState :: PlayerMap → Deck → GameState 'DealingCards
  OfferingInsuranceState :: GameContext → GameState 'OfferingInsurance
  ResolvingInsuranceState :: GameContext → GameState 'ResolvingInsurance
  OpeningTurnState :: OpeningContext → GameState 'OpeningTurn
  PlayerTurnState :: InsuranceContext → GameState 'PlayerTurn
  DealerTurnState :: InsuranceContext → GameState 'DealerTurn
  ResolvingState :: ResolutionContext → GameState 'ResolvingHands
  ResultState :: PlayerMap → GameState 'Result
  ExitedState :: GameState 'GameOver
```

This ensures that:
- **`LobbyState`** can only exist when `phase ~ 'InLobby`
- **`DealingState`** requires both players and a deck
- **`PlayerTurnState`** includes insurance context from previous phases
- **Pattern matching** on state automatically **refines the phase type**

#### Type-Safe Topology Definition

The state machine topology is defined using **Template Haskell** and **singletons** to lift the transition graph to the type level:

```haskell
$( singletons [d|
     gameTopology :: Topology GamePhase
     gameTopology = Topology
       [ (InLobby, [AwaitingBets])
       , (AwaitingBets, [DealingCards])
       , (DealingCards, [OfferingInsurance, OpeningTurn])
       , (OfferingInsurance, [ResolvingInsurance])
       , (ResolvingInsurance, [OpeningTurn, ResolvingHands])
       , (OpeningTurn, [PlayerTurn, DealerTurn, ResolvingHands])
       , (PlayerTurn, [DealerTurn, ResolvingHands])
       , (DealerTurn, [ResolvingHands])
       , (ResolvingHands, [Result])
       , (Result, [InLobby, GameOver])
       ]
   |])
```

This creates both:
- **Value-level topology** for runtime state machine execution
- **Type-level topology** for compile-time transition validation

#### Compositional Machine Architecture

CREM enables **compositional state machine design** where multiple machines can be combined using categorical combinators:

```haskell
-- Base game logic
stateMachine :: StdGen → StateMachine Command Decision

-- Automatic resolution policy (separate concern)
autoPolicy :: StateMachine Decision [Command]  
autoPolicy = statelessBase \case
  BettingEvt BetPlaced{}     → [DealingCmd DealInitialCards]
  InsuranceEvt InsuranceResolved{} → [ResolutionCmd ResolveRound]
  PlayerTurnEvt{}            → [DealerTurnCmd DealerPlay, ResolutionCmd ResolveRound]
  DealerTurnEvt{}            → [ResolutionCmd ResolveRound]
  _                          → []

-- Feedback composition: output of main machine feeds policy machine
stateMachineWithAuto :: StdGen → StateMachine Command [Decision]
stateMachineWithAuto stdGen = 
  let stateMachine' = rmap singleton (stateMachine stdGen)
  in Feedback stateMachine' autoPolicy
```

**Parallel composition** allows multiple read models:

```haskell
-- Game statistics projection
gameProjection :: BaseMachine ProjectionTopology Event Summary

-- Combined write and read models  
whole :: StdGen → StateMachine Command (Decision, Summary)
whole stdGen = stateMachine stdGen &&& projection
```

### CREM State Machine Architecture

### Command-Event Architecture

The system follows a clean **Command Query Responsibility Segregation (CQRS)** pattern:

- **Commands**: External inputs that request state changes
- **Events**: Immutable facts about what happened  
- **Decisions**: Either successful events or error states

```haskell
type Decision = Either GameError Event

-- Commands are requests that may fail
data PlayerTurnCommand 
  = Hit PlayerId | Stand PlayerId | DoubleDown PlayerId 
  | Split PlayerId | Surrender PlayerId

-- Events are successful outcomes  
data PlayerTurnEvent
  = HitCard PlayerId Card | PlayerStood PlayerId
  | PlayerDoubledDown PlayerId Card
  | PlayerSplitHand PlayerId Card Card Card Card
  | PlayerSurrendered PlayerId
```

### Threepenny Reactive Model

The web interface demonstrates **Functional Reactive Programming** principles using Threepenny GUI:

```haskell
-- Reactive Model-Update-View architecture
setupGui :: Window → UI ()
setupGui window = void mdo
  rng ← initStdGen
  let initialGame = stateMachineWithAuto rng

  -- Reactive pipeline: UI events → Commands → State updates → View updates
  (ui, EventStream commands) ← runComponent (view model)
  (decisions, _) ← mapAccum initialGame (fmap runGame commands)  
  model ← accumB initialModel (flip (foldr update) <$> decisions)

  getBody window # set children [ui]
```

The reactive model ensures that:
- **State flows unidirectionally** from user interactions through the state machine to view updates
- **Updates are atomic** and always result in consistent state
- **Side effects are contained** within the state machine transitions

### Compositional Machine Design

The application demonstrates CREM's compositional capabilities:

```haskell
-- Base game logic
stateMachine :: StdGen → StateMachine Command Decision

-- Automatic resolution policies  
autoPolicy :: StateMachine Decision [Command]

-- Composed system with automatic progression
stateMachineWithAuto :: StdGen → StateMachine Command [Decision]
stateMachineWithAuto stdGen = 
  let stateMachine' = rmap singleton (stateMachine stdGen)
  in Feedback stateMachine' autoPolicy
```

Multiple state machines can be composed using CREM's **Feedback** and **Parallel** combinators, allowing for:
- **Policy injection**: Automated decision-making layers
- **Audit trails**: Separate machines for logging and monitoring  
- **Model projections**: Read-only views for different user interfaces

## 🧪 Property-Based Testing with Hedgehog

The project demonstrates sophisticated **property-based testing** using the Hedgehog library, which is more modern and powerful than QuickCheck. Instead of writing specific test cases, we define **generators** for random test data and **properties** that should hold for all inputs.

### Generator Composition

The testing strategy uses **compositional generators** that build complex game states from simpler components:

```haskell
-- Basic generators
genCard :: Gen Card
genCard = liftA2 Card genRank genSuit

genHand :: Gen Hand  
genHand = Hand <$> Gen.list (Range.linear 2 6) genCard

-- Specialized generators for specific scenarios
genBlackjackHand :: Gen Hand
genBlackjackHand = do
  ten <- Card <$> Gen.element [Ten, Jack, Queen, King] <*> genSuit
  ace <- Card Ace <$> genSuit
  Gen.element [Hand [ace, ten], Hand [ten, ace]]

genTwoCardHand :: Gen Hand
genTwoCardHand = Hand <$> replicateM 2 genCard
```

**Constrained generators** ensure test data meets domain requirements:

```haskell
-- Generate valid bets based on available chips
genBet :: Chips -> Gen Bet
genBet maxChips = Bet <$> Gen.int (Range.linear 1 maxChips)

-- Generate player with valid stack
genPlayer :: Gen Player
genPlayer = do
  pid <- genPlayerId
  chips <- genChips
  bet <- genBet chips
  name <- genPlayerName
  pure $ Player pid (PlayerStack bet chips) name
```

**State-specific generators** create valid game states for each phase:

```haskell
-- Generate game in betting state with players who haven't bet
genBettingStateGame :: Gen (Game 'AwaitingBets)
genBettingStateGame = do
  stdGen <- genStdGen
  playerId <- genNextPlayerId  
  players <- Gen.filter (not . null) genPlayerMap
  pure $ Game stdGen playerId (BettingState players)

-- Generate complex multi-phase state
genOpeningTurnStateGame :: Gen (Game 'OpeningTurn)
genOpeningTurnStateGame = do
  game <- genPlayerTurnStateGame
  insuranceContext <- genInsuranceContext
  readyPlayers <- Gen.subset (Map.keysSet insuranceContext.context.rounds)
  pure $ game { state = OpeningTurnState (OpeningContext insuranceContext readyPlayers) }
```

### Property Categories

#### 1. **Domain Rule Properties**

Verify that game rules are correctly implemented:

```haskell
-- Blackjack hands always score 21 and have exactly 2 cards
prop_blackjack_requires_two_cards :: Property
prop_blackjack_requires_two_cards = property do
  hand <- forAll genHand
  assert (not (isBlackjack hand) || handSize hand == 2)

prop_blackjack_value_is_21 :: Property  
prop_blackjack_value_is_21 = property do
  hand <- forAll genTwoCardHand
  if isBlackjack hand
    then assert (score hand == 21)
    else success

-- Ace handling is correct for all possible hands
prop_all_aces_hand_values_correctly :: Property
prop_all_aces_hand_values_correctly = property do
  n <- forAll $ Gen.int (Range.linear 1 10)
  suit <- forAll genSuit
  let hand = Hand $ replicate n (Card Ace suit)
      expected = if n == 1 then 11 else 11 + (n - 1)
  score hand === expected
```

#### 2. **State Transition Properties**

Ensure state transitions preserve invariants and only occur when valid:

```haskell
-- Betting updates player state correctly
prop_evolve_BetPlaced_advances_state :: Property
prop_evolve_BetPlaced_advances_state = property do
  game@Game{state = BettingState players} <- forAll genBettingStateGame
  pid <- forAll (Gen.element (Map.keys players))
  bet <- forAll (genBet 1000)
  let evolved = evolveBetting game (BetPlaced pid bet)
  case evolved of
    EvolutionResult Game{state = BettingState players'} -> do
      let player = players' Map.! pid
      currentBet (stack player) === bet
    EvolutionResult Game{state = DealingState players' _} -> do
      assert $ all ((> 0) . currentBet . stack) players'
    _ -> failure

-- Card dealing preserves deck integrity
prop_hit_player_turn_draws_card :: Property
prop_hit_player_turn_draws_card = property do
  game@Game{state = PlayerTurnState InsuranceContext{context = GameContext deck rounds _}} 
    <- forAll genPlayerTurnStateGame
  pid <- forAll $ Gen.element (Map.keys rounds)
  (card, _) <- maybe discard pure (drawCard deck)
  decidePlayerTurn game (Hit pid) === Right (HitCard pid card)
```

#### 3. **Resource Conservation Properties**

Verify that game resources (cards, chips) are properly tracked:

```haskell
-- Card count increases when hitting
prop_evolve_HitCard :: Property  
prop_evolve_HitCard = property do
  (game, pid, round) <- forAll genOpeningTurnStateUnplayedHand
  card <- forAll genCard
  let evolved = evolveOpeningTurn game (HitCard pid card)
  case evolved of
    EvolutionResult Game{state = OpeningTurnState context} -> do
      let rounds = context.insuranceContext.context.rounds
          round' = rounds Map.! pid
          cardCount = handSize $ hand (Z.current (hands round))
          cardCount' = handSize $ hand (Z.current (hands round'))
      cardCount + 1 === cardCount'
    _ -> failure

-- Chip calculations are accurate
prop_chipsDelta_grouped :: Property
prop_chipsDelta_grouped = property do
  bet <- forAll $ genBet 1000
  
  label "PlayerWins Blackjack"  
  chipsDelta bet (PlayerWins Blackjack) === round (fromIntegral bet.current * 1.5 :: Float)
  
  label "Surrendered loses half"
  chipsDelta bet (DealerWins Surrendered) === -(bet.current `div` 2)
```

#### 4. **Error Handling Properties**

Ensure invalid operations are properly rejected:

```haskell
-- Cannot split non-matching cards
prop_split_reject_bad_split_hand :: Property
prop_split_reject_bad_split_hand = property do
  game <- forAll genOpeningTurnStateGame
  pid <- forAll $ Gen.element (Map.keys rounds)
  card1 <- forAll genCard
  card2 <- forAll $ Gen.filter ((/= rank card1) . rank) genCard
  let modifiedGame = -- setup game with mismatched cards
  decidePlayerTurn modifiedGame (Split pid) === Left BadCommand

-- Cannot act twice in same turn  
prop_reject_double_surrender :: Property
prop_reject_double_surrender = property do
  game <- forAll genOpeningTurnStateGame
  pid <- forAll $ Gen.element (Map.keys rounds)
  let readyPlayers = Set.fromList [pid]  -- Player already acted
      game' = game{state = OpeningTurnState (OpeningContext insuranceContext readyPlayers)}
  decidePlayerTurn game' (Surrender pid) === Left PlayerAlreadyActed
```

### Advanced Testing Patterns

#### **Conditional Properties**

Test properties that only apply under certain conditions:

```haskell
prop_dealer_hits_under_17 :: Property
prop_dealer_hits_under_17 = property do
  dealer@(Dealer hand) <- forAll genDealer
  (score hand < 17) === dealerShouldHit dealer
```

#### **Multi-Step State Evolution**

Test complex scenarios involving multiple state transitions:

```haskell
prop_evolve_CardsDealt_advances_state :: Property
prop_evolve_CardsDealt_advances_state = property do
  game@Game{state = DealingState players deck} <- forAll genDealingStateGame
  playerHands <- forAll $ genValidPlayerHands players
  dealer <- forAll genDealer
  let evolved = evolveDealing game (CardsDealt playerHands dealer)
  case evolved of
    EvolutionResult Game{state = OfferingInsuranceState ctx} -> do
      assert (hasAce ctx.dealer.dealerHand)  -- Insurance only offered with Ace
      length ctx.rounds === length playerHands
    EvolutionResult Game{state = OpeningTurnState ctx} -> do
      assert (null ctx.readyPlayers)  -- Fresh turn state
      length ctx.insuranceContext.context.rounds === length playerHands
    _ -> failure
```

#### **Generator Combinators**

Custom generators for complex test scenarios:

```haskell
-- Generate game state with an unplayed hand for testing
genOpeningTurnStateUnplayedHand :: Gen (Game 'OpeningTurn, PlayerId, PlayerRound)
genOpeningTurnStateUnplayedHand = do
  game@Game{state = OpeningTurnState context} <- genOpeningTurnStateGame
  (pid, round) <- Gen.element (Map.toList context.insuranceContext.context.rounds)
  newHandState <- genUnplayedHand
  let currentHand = Z.current (hands round)
      currentHand' = currentHand{hasDoubledDown = False, hasStood = False}
      hands' = Z.push newHandState (Z.replace currentHand' $ hands round)
      round' = round{hands = hands', hasSurrendered = False}
      -- Update game state with modified round
  pure (updatedGame, pid, round')
  where
    genUnplayedHand = HandState <$> genTwoCardHand <*> genBet 1000 <*> pure False <*> pure False
```

### Test Organization

Tests are organized by game phase and concern:

```haskell
-- Game.Test.Betting: Properties for bet placement phase
-- Game.Test.Dealing: Properties for card distribution
-- Game.Test.PlayerTurn: Properties for player decisions
-- Game.Test.DealerTurn: Properties for dealer automation
-- Game.Test.Resolution: Properties for outcome calculation
-- Game.TypesSpec: Properties for domain types and rules
```

**Template Haskell** automatically discovers properties:

```haskell
tests :: IO Bool
tests = checkParallel $$discover  -- Finds all prop_* functions
```

This comprehensive testing approach ensures that:
- **All game rules** are correctly implemented across all possible inputs
- **State transitions** maintain consistency and respect domain constraints  
- **Edge cases** are automatically discovered through random generation
- **Refactoring** is safe because properties act as a comprehensive regression suite
- **Documentation** exists in the form of executable specifications

## 📦 Project Structure

```
├── src/
│   ├── GameTopology.hs      # CREM state machine definition
│   ├── Game.hs              # Core game logic and decider
│   ├── Types.hs             # Domain types and data structures  
│   ├── Application.hs       # Machine composition and policies
│   └── Game/
│       ├── Lobby.hs         # Player management phase
│       ├── Betting.hs       # Bet placement logic
│       ├── Dealing.hs       # Card distribution
│       ├── Insurance.hs     # Insurance bet handling
│       ├── PlayerTurn.hs    # Player decision processing
│       ├── DealerTurn.hs    # Automated dealer play
│       ├── Resolution.hs    # Hand outcome calculation
│       └── Result.hs        # Game conclusion logic
├── webapp/
│   ├── Main.hs              # Threepenny GUI entry point
│   └── Game/UI/
│       ├── Model.hs         # Reactive model state
│       ├── Component.hs     # UI component primitives
│       └── View.hs          # View rendering logic
├── app/
│   └── Main.hs              # Console interface
└── test/
    ├── Spec.hs              # Test suite entry point
    ├── Game/Gen.hs          # Hedgehog generators
    └── Game/Test/           # Property-based tests
```

## 🚀 Getting Started

### Prerequisites

- **GHC 9.4+** with **GHC2021** language extensions
- **Stack** or **Cabal** for dependency management

### Installation

```bash
# Clone the repository
git clone https://github.com/beefyhalo/blackjack.git
cd blackjack

# Install dependencies
stack install  # or cabal install
```

### Running the Application

#### Console Interface
```bash
# Interactive terminal game
stack exec blackjack
```

#### Web Interface  
```bash  
# Launch web server (default: http://localhost:8023)
stack exec blackjack-webapp

# Then open your browser to play
```

#### Running Tests
```bash
# Execute property-based test suite
stack test

# Run with verbose output
stack test --test-arguments="--verbose"
```

### Build Options

```bash
# Development build
stack build

# Optimized release build  
stack build --ghc-options="-O2"

# Generate documentation
stack haddock
```

## 🎯 Key Learning Outcomes

This project demonstrates several advanced functional programming concepts:

### 1. **Type-Level Programming**
- Phantom types for compile-time state safety
- GADTs for type-safe pattern matching
- Type families for dependent types

### 2. **Functional Reactive Programming**  
- Event streams and behaviors
- Compositional UI components
- Unidirectional data flow

### 3. **Domain Modeling**
- Algebraic data types for precise domain representation
- Smart constructors and invariant preservation
- Error handling with `Either` types

### 4. **Testing Strategies**
- Property-based testing with Hedgehog
- Generator composition and combinators
- Invariant verification across state spaces

### 5. **Software Architecture**
- CQRS (Command Query Responsibility Segregation)
- State machine composition patterns  
- Separation of concerns through type boundaries

## 🔧 Advanced Usage

### Custom Policies

Extend the game with custom automated policies:

```haskell
-- Custom insurance policy
conservativeInsurance :: BaseMachine Event (Maybe Command)
conservativeInsurance = -- Implementation that declines all insurance

-- Compose with main game logic  
gameWithPolicy :: StdGen → StateMachine Command [Event]
gameWithPolicy stdGen = 
  Feedback (stateMachine stdGen) conservativeInsurance
```

### Multiple Projections

Create different read models for various interfaces:

```haskell
-- Admin dashboard projection
adminProjection :: StateMachine Event AdminSummary

-- Player statistics projection  
statsProjection :: StateMachine Event PlayerStats

-- Combine multiple projections
dashboardMachine :: StdGen → StateMachine Command (Decision, AdminSummary, PlayerStats)
dashboardMachine stdGen = 
  stateMachine stdGen &&& adminProjection &&& statsProjection
```

## 📚 Further Reading

- **[CREM Library](https://hackage.haskell.org/package/crem)**: Compositional Representable Executable Machines
- **[Threepenny GUI](https://hackage.haskell.org/package/threepenny-gui)**: Haskell web GUI framework  
- **[Hedgehog](https://hackage.haskell.org/package/hedgehog)**: Modern property-based testing
- **[Type-Safe State Machines](https://wickstrom.tech/finite-state-machines/2017/11/10/finite-state-machines-part-1-modeling-with-haskell.html)**: Modeling with Haskell types

## 🤝 Contributing

Contributions are welcome! This project serves as both a functional game and a demonstration of advanced Haskell techniques. Areas for enhancement:

- **Additional Game Variants**: European Blackjack, Spanish 21, etc.
- **Advanced Strategies**: Card counting, basic strategy hints  
- **Performance Optimizations**: Streaming, parallel processing
- **UI Enhancements**: Animations, sound effects, mobile support

Please ensure all contributions include appropriate property-based tests and maintain the type-safe design principles.

## 📄 License

This project is licensed under the **BSD 3-Clause License** - see the [LICENSE](LICENSE) file for details.

---

**Built with ❤️ in Haskell, demonstrating the power of functional programming, type safety, and compositional design.**
