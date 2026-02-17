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
projection :: BaseMachine ProjectionTopology Event Summary

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
setupGui :: Window → UI ()
setupGui window = void mdo
  rng ← initStdGen
  let initialGame = stateMachineWithAuto rng

  -- Reactive Model-Update-View
  (ui, EventStream commands) ← runComponent (view model)
  (decisions, _) ← mapAccum initialGame (runGame <$> commands)  
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

The project demonstrates **property-based testing** using the Hedgehog library, which is more modern and powerful than QuickCheck. Instead of writing specific test cases, we define **generators** for random test data and **properties** that should hold for all inputs.

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

Nix is optional but recommended.

### Installation

```bash
# Clone the repository
git clone https://github.com/beefyhalo/blackjack.git
cd blackjack

# Enter the dev shell (includes GHC, cabal, HLS, etc.)
nix develop
```

### Running the Application

```bash
# Interactive terminal game
cabal run blackjack
```

```bash
# Launch web server (default: http://localhost:8023)
cabal run blackjack-webapp
```

Then open your browser to play.

### Running Tests

```bash
# Execute property-based test suite
nix develop -c cabal test
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

## 📚 Further Reading

- **[CREM Library](https://hackage.haskell.org/package/crem)**: Compositional Representable Executable Machines
- **[Threepenny GUI](https://hackage.haskell.org/package/threepenny-gui)**: Haskell web GUI framework  
- **[Hedgehog](https://hackage.haskell.org/package/hedgehog)**: Modern property-based testing

**Built with ❤️ in Haskell, demonstrating the power of functional programming, type safety, and compositional design.**
