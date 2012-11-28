{
module Language.Haskell.TinySig.Happy where

import Language.Haskell.TinySig.Alex
import Language.Haskell.TinySig.AST
}

%name                     parseTinySig TinySig
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

  Ident                   { IdentTk $$ }

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
  : Ident                 { IdentS $1 }
  | "(" TinySig ")"       { $2 }
  ;

List
  : Atom                  { $1 }
  | "[" Atom              { ListS $2 }
  ;

Product
  : sep1(List, ",")       { oneOrA ProductS $1 }
  ;

Sum
  : sep1(Product, "+")    { oneOrA SumS $1 }
  ;

DashFunction
  : sep1(Sum, "-")        { oneOrA funS $1 }
  ;

DotFunction
  : sep1(DashFunction, ".")
                          { oneOrA funS $1 }
  ;

TinySig
  : DotFunction           { $1 }
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
}
