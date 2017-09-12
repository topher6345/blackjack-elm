# Blackjack in Elm

## Getting started

You need to have [Elm](http://elm-lang.org/) 0.18 installed on your machine.

Compile this project with:

    elm make src/Main.elm

Then view it:

    elm reactor

## Key Elm Modules and Types
in `src/`
 * `Main` - Entry Point for Elm App
 * `Card` - Dealing and Card manipulations
 * `Score.Score` - (hard, soft)
 * `Score` - operations over `Score`s
 * `Flash` - Status and Results of Game displayed to user
 * `Flash.ScoreState` - Enum of s

## TODO
- [x] Hit
- [x] Stand
- [x] Surrender
- [x] Statistics
- [x] Wager
- [ ] Split
- [ ] Dealer cards dont flip if dealer is dealt a blackjack on the first hand
- [ ] Double Down
