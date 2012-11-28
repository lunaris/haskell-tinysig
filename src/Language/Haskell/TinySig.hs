module Language.Haskell.TinySig where

import Language.Haskell.TinySig.Expand
import Language.Haskell.TinySig.Happy

import Language.Haskell.Exts hiding (Rule)

defaultRules :: [Rule]
defaultRules
  = [ ("C", simpleType "Char")
    , ("i", simpleType "Int")
    , ("I", simpleType "Integer")
    , ("M", simpleType "Maybe")
    , ("s", simpleType "String")
    ]

main :: IO ()
main
  = interact $ \s ->
      either (const s) (prettyPrint . expand defaultRules)
        (parseTinySig s)
