{-# LANGUAGE PatternGuards #-}

module Language.Haskell.TinySig.Expand where

import Language.Haskell.TinySig.AST

import Data.Char
import Data.Maybe
import Language.Haskell.Exts hiding (Rule)

type Rule
  = (Constructor, Type)

simpleType :: String -> Type
simpleType
  = TyCon . UnQual . Ident

typeVariable :: Char -> Type
typeVariable
  = TyVar . Ident . (:[])

expand :: [Rule] -> TinySig -> Type
expand rs (VariableS v)
  = typeVariable v

expand rs (ConstructorS c)
  = fromMaybe (simpleType c) (lookup c rs)

expand rs (FunS ts1 ts2)
  = TyFun (expand rs ts1) (expand rs ts2)

expand rs (StarS ts)
  = TyList (expand rs ts)

expand rs (OptionS ts)
  = TyApp (simpleType "Maybe") (expand rs ts)

expand rs (ProductS tss)
  = TyTuple Boxed (map (expand rs) tss)

expand rs (SumS tss)
  = foldl1 (TyApp . TyApp (simpleType "Either")) (map (expand rs) tss)

expand rs (AppS ts1 ts2)
  = TyApp (expand rs ts1) (expand rs ts2)
