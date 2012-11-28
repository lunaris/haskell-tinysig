module Language.Haskell.TinySig.AST where

type Variable
  = Char

type Constructor
  = String

data TinySig
  = VariableS Variable
  | ConstructorS Constructor
  | FunS TinySig TinySig
  | StarS TinySig
  | OptionS TinySig
  | ProductS [TinySig]
  | SumS [TinySig]
  | DotS TinySig TinySig
  | AppS TinySig TinySig
  deriving (Eq, Show)
