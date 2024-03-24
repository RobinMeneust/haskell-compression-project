# haskell-compression-project

## Requirements

- The Haskell Tool Stack

## Warning

- The path of this project must not contain special characters or spaces
- If stack commands don't work, then use: `sudo` and `--allow-different-user`. e.g. `sudo stack build --allow-different-user`
- If it still doesn't work, you can also try to delete the folder .stack-work and re-build the project with stack

## Build

In the project folder, run: `stack build`

## Run

In the project folder, run: `stack exec haskell-compression-project-exe`

## Test

In the project folder, run: `stack test`

## Documentation

In the project folder, run: 

1. `stack build`
2. `stack haddock`
3. Go to ".stack-work/dist/x86_64-linux-tinfo6/ghc-9.6.4/doc/" (or a similar path) and open index.html

## Authors

- Nino Hamel
- Robin Meneust
- Jérémy Saëlen
- Mathis Tempo