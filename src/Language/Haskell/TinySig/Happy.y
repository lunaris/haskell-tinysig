{
module Language.Haskell.TinySig.Happy where

import Language.Haskell.TinySig.Alex
import Language.Haskell.TinySig.AST
}

%name           parseKenya Expression
%tokentype      { Token }
%error          { parseError }

%monad          { LP }
%lexer          { lexToken } { EOFTk }

%token

  "void"                  { VoidTk }
  "boolean"               { BooleanTk }
  "char"                  { CharTk }
  "String"                { StringTk }
  "int"                   { IntTk }
  "float"                 { FloatTk }
  "double"                { DoubleTk }

  "const"                 { ConstTk }

  "null"                  { NullTk }
  "true"                  { TrueTk }
  "false"                 { FalseTk }

  "class"                 { ClassTk }
  "enum"                  { EnumTk }

  "="                     { AssignTk }
  "new"                   { NewTk }

  "+"                     { PlusTk }
  "++"                    { PlusPlusTk }
  "-"                     { MinusTk }
  "--"                    { MinusMinusTk }
  "*"                     { TimesTk }
  "/"                     { DivideTk }
  "%"                     { ModTk }
  "=="                    { EqualTk }
  "!="                    { NotEqualTk }
  "<"                     { LessThanTk }
  "<="                    { LessThanEqualTk }
  ">"                     { GreaterThanTk }
  ">="                    { GreaterThanEqualTk }
  "&&"                    { AndTk }
  "||"                    { OrTk }
  "!"                     { NotTk }

  "if"                    { IfTk }
  "else"                  { ElseTk }
  "for"                   { ForTk }
  "while"                 { WhileTk }
  "do"                    { DoTk }
  "switch"                { SwitchTk }
  "case"                  { CaseTk }
  "default"               { DefaultTk }
  "return"                { ReturnTk }

  "("                     { LeftParenTk }
  ")"                     { RightParenTk }
  "["                     { LeftBracketTk }
  "]"                     { RightBracketTk }
  "{"                     { LeftBraceTk }
  "}"                     { RightBraceTk }
  ";"                     { SemiTk }
  ":"                     { ColonTk }
  ","                     { CommaTk }
  "."                     { DotTk }

  Ident                   { IdentTk $$ }

  CharLiteral             { CharLiteralTk $$ }
  StringLiteral           { StringLiteralTk $$ }

  IntLiteral              { IntLiteralTk $$ }

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

Type
  : PrimitiveType         { $1 }
  | ReferenceType         { $1 }
  ;

PrimitiveType
  : "boolean"             { BooleanT }
  | "char"                { CharT }
  | "int"                 { IntT }
  | "float"               { FloatT }
  | "double"              { DoubleT }
  ;

ReferenceType
  : ClassType             { $1 }
  | ArrayType             { $1 }
  ;

ClassType
  : Ident                 { ClassT $1 }
  ;

ArrayType
  : Type "[" "]"          { ArrayT $1 }
  ;

BooleanLiteral
  : "true"                { BooleanE True }
  | "false"               { BooleanE False }

Literal
  : "null"                { NullE }
  | BooleanLiteral        { $1 }
  | CharLiteral           { CharE $1 }
  | StringLiteral         { StringE $1 }
  | IntLiteral            { IntE $1 }
  ;

Primary
  : NonNewArrayPrimary    { $1 }
  | ArrayCreation         { ArrayE $1 }

NonNewArrayPrimary
  : Literal               { $1 }
  | "(" Expression ")"    { $2 }
  | InstanceCreation      { $1 }
  | FieldAccess           { $1 }
  | MethodInvocation      { $1 }
  | ArrayAccess           { $1 }
  ;

FieldAccess
  : Primary "." Ident     { FieldE $1 $3 }
  ;

MethodInvocation
  : Ident "(" sep(Expression, ",") ")"
      { AppE $1 $3 }

  ;

ArrayAccess
  : Name "[" Expression "]"
      { IndexE $1 $3 }

  | NonNewArrayPrimary "[" Expression "]"
      { IndexE $1 $3 }

  ;

ArrayCreation
  : "new" PrimitiveType many1(SizedDimension) many(Dimension)
      { DefaultAE $2 $3 (length $4) }

  | "new" ClassType many1(SizedDimension) many(Dimension)
      { DefaultAE $2 $3 (length $4) }

  | "new" PrimitiveType many1(Dimension) ArrayInitialiser
      { InitAE $2 (length $3) $4 }

  | "new" ClassType many1(Dimension) ArrayInitialiser
      { InitAE $2 (length $3) $4 }

  ;

SizedDimension
  : "[" Expression "]"    { $2 }
  ;

Dimension
  : "[" "]"               { () }
  ;

ArrayInitialiser
  : "{" sep(VariableInitialiser, ",") opt(",") "}"
      { ArrayI $2 }

  ;

VariableInitialiser
  : Expression            { ScalarI $1 }
  | ArrayInitialiser      { $1 }
  ;

InstanceCreation
  : "new" Ident           { NewE $2 }
  ;

PostfixExpression
  : Primary               { $1 }
  | Name                  { $1 }
  | PostfixIncExpression  { $1 }
  | PostfixDecExpression  { $1 }
  ;

PostfixIncExpression
  : PostfixExpression "++"
      { UnOpE PostfixIncO $1 }

  ;

PostfixDecExpression
  : PostfixExpression "--"
      { UnOpE PostfixDecO $1 }

  ;

Name
  : Ident                 { VarE $1 }
  ;

PrefixIncExpression
  : "++" UnaryExpression  { UnOpE PrefixIncO $2 }
  ;

PrefixDecExpression
  : "--" UnaryExpression  { UnOpE PrefixDecO $2 }
  ;

UnaryExpression
  : PrefixIncExpression   { $1 }
  | PrefixDecExpression   { $1 }
  | "!" UnaryExpression   { UnOpE NotO $2 }
  | PostfixExpression     { $1 }
  ;

MultiplicativeExpression
  : UnaryExpression
      { $1 }

  | MultiplicativeExpression "*" UnaryExpression
      { BinOpE MulO $1 $3 }

  | MultiplicativeExpression "/" UnaryExpression
      { BinOpE DivO $1 $3 }

  | MultiplicativeExpression "%" UnaryExpression
      { BinOpE ModO $1 $3 }

  ;

AdditiveExpression
  : MultiplicativeExpression
      { $1 }

  | AdditiveExpression "+" MultiplicativeExpression
      { BinOpE AddO $1 $3 }

  | AdditiveExpression "-" MultiplicativeExpression
      { BinOpE SubO $1 $3 }

  ;

RelationalExpression
  : AdditiveExpression
      { $1 }

  | RelationalExpression "<" AdditiveExpression
      { BinOpE LtO $1 $3 }

  | RelationalExpression ">" AdditiveExpression
      { BinOpE GtO $1 $3 }

  | RelationalExpression "<=" AdditiveExpression
      { BinOpE LtEqO $1 $3 }

  | RelationalExpression ">=" AdditiveExpression
      { BinOpE GtEqO $1 $3 }

  ;

EqualityExpression
  : RelationalExpression
      { $1 }

  | EqualityExpression "==" RelationalExpression
      { BinOpE EqO $1 $3 }

  | EqualityExpression "!=" RelationalExpression
      { BinOpE NotEqO $1 $3 }

  ;

ConditionalAndExpression
  : EqualityExpression
      { $1 }

  | ConditionalAndExpression "&&" EqualityExpression
      { BinOpE AndO $1 $3 }

  ;

ConditionalOrExpression
  : ConditionalAndExpression
      { $1 }

  | ConditionalOrExpression "||" ConditionalAndExpression
      { BinOpE OrO $1 $3 }

  ;

AssignmentExpression
  : ConditionalOrExpression
      { $1 }

  | Assignment
      { $1 }

  ;

Assignment
  : AssignmentLHS "=" AssignmentExpression
      { BinOpE AssignO $1 $3 }

  ;

AssignmentLHS
  : Name                  { $1 }
  | FieldAccess           { $1 }
  | ArrayAccess           { $1 }
  ;

Expression
  : AssignmentExpression  { $1 }

{
parseError :: Token -> LP a
parseError _
  = fail "Parse error!"
}
