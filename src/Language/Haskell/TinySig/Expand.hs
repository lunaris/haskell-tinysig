{-# LANGUAGE PatternGuards #-}

module Language.Haskell.TinySig.Expand where

import Language.Haskell.TinySig.AST

import Data.Char
import Data.Maybe
import Language.Haskell.Exts hiding (Rule)

type Rule
  = (Ident, Type)

simpleType :: String -> Type
simpleType
  = TyCon . UnQual . Ident

typeVariable :: Char -> Type
typeVariable
  = TyVar . Ident . (:[])

expand :: [Rule] -> TinySig -> Type
expand rs (IdentS ident)
  | Just ty <- lookup ident rs = ty

expand rs (IdentS ident@(c : cs))
  | isUpper c = simpleType ident
  | null cs   = ty
  | otherwise = TyApp ty (expand rs (IdentS cs))
  where
    ty = typeVariable c

expand rs (FunS ts1 ts2)
  = TyFun (expand rs ts1) (expand rs ts2)

expand rs (ListS ts)
  = TyList (expand rs ts)

expand rs (ProductS tss)
  = TyTuple Boxed (map (expand rs) tss)

expand rs (SumS tss)
  = foldl1 (TyApp . TyApp (simpleType "Either")) (map (expand rs) tss)

expand rs (AppS ts1 ts2)
  = TyApp (expand rs ts1) (expand rs ts2)
