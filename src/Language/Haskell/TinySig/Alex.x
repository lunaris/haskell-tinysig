{
module Language.Haskell.TinySig.Alex where

import Language.Haskell.TinySig.Position

import Control.Monad
import Data.Word
}

@reserved                   = "abstract" | "strictfp" | "interface" | "super"
                            | "extends" | "long" | "byte" | "final" | "native"
                            | "synchronized" | "finally" | "this" | "catch"
                            | "package" | "throw" | "private" | "throws"
                            | "goto" | "protected" | "transient" | "public"
                            | "try" | "continue" | "implements" | "import"
                            | "short" | "volatile" | "instanceof" | "static"

$digit                      = [0-9]
$letter                     = [a-zA-Z_]
$hexDigit                   = [0-9a-fA-F]

@decimal                    = 0 | [1-9] $digit*
@hexadecimal                = 0 [xX] $hexDigit+

@char                       = "'" $printable "'"
@string                     = \" ($printable # \")* \"

tokens :-
  $white+                   ;
  "//".*                    ;

  @reserved                 { reserved }

  "void"                    { token VoidTk }
  "boolean"                 { token BooleanTk }
  "char"                    { token CharTk }
  "String"                  { token StringTk }
  "int"                     { token IntTk }
  "float"                   { token FloatTk }
  "double"                  { token DoubleTk }

  "const"                   { token ConstTk }

  "null"                    { token NullTk }
  "true"                    { token TrueTk }
  "false"                   { token FalseTk }

  "class"                   { token ClassTk }
  "enum"                    { token EnumTk }

  "="                       { token AssignTk }
  "new"                     { token NewTk }

  "+"                       { token PlusTk }
  "++"                      { token PlusPlusTk }
  "-"                       { token MinusTk }
  "--"                      { token MinusMinusTk }
  "*"                       { token TimesTk }
  "/"                       { token DivideTk }
  "%"                       { token ModTk }
  "=="                      { token EqualTk }
  "!="                      { token NotEqualTk }
  "<"                       { token LessThanTk }
  "<="                      { token LessThanEqualTk }
  ">"                       { token GreaterThanTk }
  ">="                      { token GreaterThanEqualTk }
  "&&"                      { token AndTk }
  "||"                      { token OrTk }
  "^"                       { token XorTk }
  "!"                       { token NotTk }

  "if"                      { token IfTk }
  "else"                    { token ElseTk }
  "for"                     { token ForTk }
  "while"                   { token WhileTk }
  "do"                      { token DoTk }
  "switch"                  { token SwitchTk }
  "case"                    { token CaseTk }
  "default"                 { token DefaultTk }
  "return"                  { token ReturnTk }

  "("                       { token LeftParenTk }
  ")"                       { token RightParenTk }
  "["                       { token LeftBracketTk }
  "]"                       { token RightBracketTk }
  "{"                       { token LeftBraceTk }
  "}"                       { token RightBraceTk }
  ";"                       { token SemiTk }
  ":"                       { token ColonTk }
  ","                       { token CommaTk }
  "."                       { token DotTk }

  $letter [$letter $digit]* { identifier }

  @char                     { char }
  @string                   { string }

  @decimal                  { decimal }
  @hexadecimal              { hexadecimal }
{
type AlexInput
  = (Position, Word8, String)

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (_, _, [])
  = Nothing

alexGetByte (pos, _, (c : cs))
  = Just (w, (movePosition pos c, w, cs))
    where
      w = fromIntegral (ord c)

alexInputPrevChar :: AlexInput -> Word8
alexInputPrevChar (_, w, _)
  = w

type StartCode
  = Int

data LPState
  = LPState { lpPosition  :: !Position
            , lpPrevious  :: !Word8
            , lpInput     :: String
            , lpStartCode :: !StartCode
            }
  deriving (Eq, Show)

newtype LP a
  = LP { runLP :: LPState -> Either String (a, LPState) }

evalLP :: LP a -> String -> Either String a
evalLP (LP f) input
  = case f initial of
      Left err ->
        Left err

      Right (x, _) ->
        Right x

    where
      initial
        = LPState { lpPosition  = initialPosition
                  , lpPrevious  = 0
                  , lpInput     = input
                  , lpStartCode = 0
                  }

instance Functor LP where
  {-# INLINE fmap #-}
  fmap
    = liftM

instance Monad LP where
  {-# INLINE (>>=) #-}
  m >>= f
    = LP $ \s ->
        case runLP m s of
          Left err ->
            Left err

          Right (x, s') ->
            runLP (f x) s'

  {-# INLINE return #-}
  return x
    = LP $ \s -> Right (x, s)

getAlexInput :: LP AlexInput
getAlexInput
  = LP $ \s@LPState { lpPosition = pos, lpPrevious = w, lpInput = input } ->
      Right ((pos, w, input), s)

setAlexInput :: AlexInput -> LP ()
setAlexInput (pos, w, input)
  = LP $ \s ->
      Right ((), s { lpPosition = pos, lpPrevious = w, lpInput = input })

getPosition :: LP Position
getPosition
  = LP $ \s@LPState { lpPosition = pos } -> Right (pos, s)

getStartCode :: LP StartCode
getStartCode
  = LP $ \s@LPState { lpStartCode = start_code } -> Right (start_code, s)

setStartCode :: StartCode -> LP ()
setStartCode start_code
  = LP $ \s ->
      Right ((), s { lpStartCode = start_code })

scanToken :: LP Token
scanToken = do
  alex_input@(_, _, input) <- getAlexInput
  start_code <- getStartCode

  case alexScan alex_input start_code of
    AlexEOF ->
      return EOFTk

    AlexError _ ->
      LP $ \_ -> Left "Lexical error"

    AlexSkip alex_input' _ -> do
      setAlexInput alex_input'
      scanToken

    AlexToken alex_input' len action -> do
      setAlexInput alex_input'
      action (take len input)

lexToken :: (Token -> LP a) -> LP a
lexToken k
  = scanToken >>= k

type Action a
  = String -> LP a

type Ident
  = String

{-# INLINE identifier #-}
identifier :: Action Token
identifier input
  = return (IdentTk input)

data Token
  = ReservedTk String

  | VoidTk
  | BooleanTk
  | CharTk
  | StringTk
  | IntTk
  | FloatTk
  | DoubleTk

  | ConstTk

  | NullTk
  | TrueTk
  | FalseTk

  | ClassTk
  | EnumTk

  | AssignTk
  | NewTk

  | PlusTk
  | PlusPlusTk
  | MinusTk
  | MinusMinusTk
  | TimesTk
  | DivideTk
  | ModTk
  | EqualTk
  | NotEqualTk
  | LessThanTk
  | LessThanEqualTk
  | GreaterThanTk
  | GreaterThanEqualTk
  | AndTk
  | OrTk
  | XorTk
  | NotTk

  | IfTk
  | ElseTk
  | ForTk
  | WhileTk
  | DoTk
  | SwitchTk
  | CaseTk
  | DefaultTk
  | ReturnTk

  | LeftParenTk
  | RightParenTk
  | LeftBracketTk
  | RightBracketTk
  | LeftBraceTk
  | RightBraceTk
  | SemiTk
  | ColonTk
  | CommaTk
  | DotTk

  | IdentTk Ident

  | CharLiteralTk Char
  | StringLiteralTk String
  | IntLiteralTk Int
  | DoubleLiteralTk Double

  | EOFTk
  deriving (Eq, Show)

{-# INLINE reserved #-}
reserved :: Action Token
reserved input
  = return (ReservedTk input)

{-# INLINE token #-}
token :: Token -> Action Token
token tok _
  = return tok

{-# INLINE char #-}
char :: Action Token
char input
  = return (CharLiteralTk (head (tail (init input))))

{-# INLINE string #-}
string :: Action Token
string input
  = return (StringLiteralTk (tail (init input)))

{-# INLINE decimal #-}
decimal :: Action Token
decimal input
  = return (IntLiteralTk (read input))

{-# INLINE hexadecimal #-}
hexadecimal :: Action Token
hexadecimal input
  = return (IntLiteralTk (read input))
}
