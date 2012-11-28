module Language.Haskell.TinySig.Expand where

import Language.Haskell.TinySig.AST

import Data.Maybe
import Language.Haskell.Exts hiding (Rule)

type Rule
  = (Ident, Type)

simpleType :: String -> Type
simpleType
  = TyCon . UnQual . Ident

typeVariable :: String -> Type
typeVariable
  = TyVar . Ident

expand :: [Rule] -> TinySig -> Type
expand rs (IdentS ident)
  = fromMaybe (typeVariable ident) (lookup ident rs)

expand rs (FunS ts1 ts2)
  = TyFun (expand rs ts1) (expand rs ts2)

expand rs (ListS ts)
  = TyList (expand rs ts)

expand rs (ProductS tss)
  = TyTuple Boxed (map (expand rs) tss)

expand rs (SumS tss)
  = foldl1 (TyApp . TyApp either) (map (expand rs) tss)
    where
      either
        = simpleType "Either"

expand rs (AppS ts1 ts2)
  = TyApp (expand rs ts1) (expand rs ts2)
