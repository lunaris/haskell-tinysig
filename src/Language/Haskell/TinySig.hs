module Language.Haskell.TinySig where

import Language.Haskell.TinySig.Expand
import Language.Haskell.TinySig.Happy

import Language.Haskell.Exts

main :: IO ()
main
  = interact $
      either id (prettyPrint . expand rs) . parseTinySig

    where
      rs
        = [ ("i", simpleType "Int")
          , ("I", simpleType "Integer")
          , ("M", simpleType "Maybe")
          , ("C", simpleType "Char")
          ]
