module Language.Haskell.TinySig.AST where

type Ident
  = String

data TinySig
  = IdentS Ident
  | FunS TinySig TinySig
  | ListS TinySig
  | ProductS [TinySig]
  | SumS [TinySig]
  | AppS TinySig TinySig
  deriving (Eq, Show)
