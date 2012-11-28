module Language.Haskell.TinySig.Position where

data Position
  = Position !Int !Int !Int
  deriving (Eq, Ord, Show)

initialPosition :: Position
initialPosition
  = Position 0 1 1

movePosition :: Position -> Char -> Position
movePosition (Position addr line column) '\t'
  = Position (addr + 1) line (((column + 7) `div` 8) * 8 + 1)

movePosition (Position addr line _) '\n'
  = Position (addr + 1) (line + 1) 1

movePosition (Position addr line column) _
  = Position (addr + 1) line (column + 1)
