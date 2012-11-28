{
module Language.Haskell.TinySig.Happy where

import Language.Haskell.TinySig.Alex
import Language.Haskell.TinySig.AST
}

%name                     tinySig TinySig
%tokentype                { Token }
%error                    { parseError }

%monad                    { LP }
%lexer                    { lexToken } { EOFTk }

%token

  "("                     { LeftParenTk }
  ")"                     { RightParenTk }
  "["                     { LeftBracketTk }
  "]"                     { RightBracketTk }
  "{"                     { LeftBraceTk }
  "}"                     { RightBraceTk }
  "<"                     { LeftAngleTk }
  ">"                     { RightAngleTk }
  "?"                     { QuestionTk }
  "!"                     { BangTk }
  "@"                     { AtTk }
  "#"                     { HashTk }
  "$"                     { DollarTk }
  "%"                     { PercentTk }
  "^"                     { CaretTk }
  "&"                     { AmpersandTk }
  "*"                     { AsteriskTk }
  "-"                     { HyphenTk }
  "+"                     { PlusTk }
  "|"                     { PipeTk }
  ";"                     { SemiTk }
  ":"                     { ColonTk }
  ","                     { CommaTk }
  "."                     { DotTk }
  "~"                     { TildeTk }

  Number                  { NumberTk $$ }
  Variable                { VariableTk $$ }
  Constructor             { ConstructorTk $$ }

%%

opt(x)
  : x                     { Just $1 }
  | {- Empty -}           { Nothing }
  ;

many(x)
  : many1(x)              { $1 }
  | {- Empty -}           { [] }
  ;

many1(x)
  : many1_(x)             { reverse $1 }
  ;

many1_(x)
  : x                     { [$1] }
  | many1_(x) x           { $2 : $1 }
  ;

sep(x, s)
  : sep1(x, s)            { $1 }
  | {- Empty -}           { [] }
  ;

sep1(x, s)
  : sep1_(x, s)           { reverse $1 }
  ;

sep1_(x, s)
  : x                     { [$1] }
  | sep1_(x, s) s x       { $3 : $1 }
  ;

--

Atom
  : Variable              { VariableS $1 }
  | Constructor           { ConstructorS $1 }
  | "(" TinySig ")"       { $2 }
  ;

Application
  : many1(Atom)           { foldl1 AppS $1 }
  ;

Star
  : Application           { $1 }
  | Application "*"       { StarS $1 }
  ;

Option
  : Star                  { $1 }
  | Star "?"              { OptionS $1 }
  ;

Product
  : Option "^" Number     { ProductS (replicate $3 $1) }
  | sep1(Option, ",")     { oneOrA ProductS $1 }
  ;

Sum
  : Number Product        { SumS (replicate $1 $2) }
  | sep1(Product, "+")    { oneOrA SumS $1 }
  ;

Function
  : sep1(Sum, ">")        { oneOrA funS $1 }

TinySig
  : Function              { $1 }
  ;

{
oneOrA :: ([a] -> a) -> [a] -> a
oneOrA _ [x]
  = x

oneOrA f xs
  = (f xs)

funS :: [TinySig] -> TinySig
funS
  = foldr1 FunS

parseError :: Token -> LP a
parseError _
  = fail "Parse error!"

parseTinySig :: String -> Either String TinySig
parseTinySig
  = evalLP tinySig
}
