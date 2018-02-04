# Blackjack in Elm

## Getting started

1. You need to have [Elm](http://elm-lang.org/) 0.18 installed on your machine.

Compile this project with:

    elm make src/Main.elm --output js/elm.js

Run the tests:

    elm test

Then view it:

    elm reactor

## Key Elm Modules and Types
in `src/`
 * `BasicStrategy` - [Basic Strategy](https://en.wikipedia.org/wiki/Blackjack#Basic_strategy) Engine
 * `BasicStrategyView` - Html elements of the Basic Strategy
 * `Card` - Dealing and Card manipulations
 * `DealerStand` - Logic for Dealer to stand at 17
 * `Flash` - Status and Results of Game displayed to user
 * `Main` - Entry Point for Elm App
 * `Score` - operations over `Score`s
 * `Score.Score` - (hard, soft)

## TODO
- [x] Hit
- [x] Stand
- [x] Surrender
- [x] Statistics
- [x] Wager
- [ ] Split
- [ ] Dealer cards dont flip if dealer is dealt a blackjack on the first hand
- [ ] Double Down
