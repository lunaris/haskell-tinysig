{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TinySig.AST where

import Data.Label

type Ident
  = String

data Type
  = VoidT
  | BooleanT
  | CharT
  | StringT
  | IntT
  | FloatT
  | DoubleT
  | ArrayT Type
  | ClassT Ident
  | EnumT Ident
  deriving (Eq, Show)

data UnOp
  = PrefixIncO
  | PostfixIncO
  | PrefixDecO
  | PostfixDecO
  | NotO
  deriving (Eq, Show)

data BinOp
  = AssignO
  | AddO
  | SubO
  | MulO
  | DivO
  | ModO
  | EqO
  | NotEqO
  | LtO
  | LtEqO
  | GtO
  | GtEqO
  | AndO
  | OrO
  | XorO
  deriving (Eq, Show)

data Initialiser
  = ScalarI     { _iExp             :: Exp }
  | ArrayI      { _iInits           :: [Initialiser] }
  deriving (Eq, Show)

data ArrayExp
  = DefaultAE   { _aeType           :: Type
                , _aeSizedDims      :: [Exp]
                , _aeDims           :: Int
                }

  | InitAE      { _aeType           :: Type
                , _aeDims           :: Int
                , _aeInit           :: Initialiser
                }
  deriving (Eq, Show)

data Exp
  = NullE
  | BooleanE    { _eBool            :: Bool }
  | CharE       { _eChar            :: Char }
  | StringE     { _eString          :: String }
  | IntE        { _eInt             :: Int }
  | DoubleE     { _eDouble          :: Double }
  | VarE        { _eIdent           :: Ident }
  | NewE        { _eIdent           :: Ident }
  | ArrayE      { _eArrayExp        :: ArrayExp }

  | UnOpE       { _eUnOp            :: UnOp
                , _eLeftExp         :: Exp
                }

  | BinOpE      { _eBinOp           :: BinOp
                , _eLeftExp         :: Exp
                , _eRightExp        :: Exp
                }

  | AppE        { _eIdent           :: Ident
                , _eArgs            :: [Exp]
                }

  | IndexE      { _eLeftExp         :: Exp
                , _eRightExp        :: Exp
                }

  | FieldE      { _eLeftExp         :: Exp
                , _eField           :: Ident
                }
  deriving (Eq, Show)

data Modifier
  = ConstM
  deriving (Eq, Show)

data Declarator
  = Declarator  { _dModifiers       :: [Modifier]
                , _dIdent           :: Ident
                , _dTypeModifier    :: Type -> Type
                , _dInit            :: Maybe Initialiser
                }

data Stmt
  = DeclS       { _sType            :: Type
                , _sDeclarators     :: [Declarator]
                }

  | ExpS        { _sExp             :: Exp }

  | IfS         { _sPredicate       :: Exp
                , _sThen            :: [Stmt]
                , _sElse            :: [Stmt]
                }

  | ForS        { _sInits           :: [Stmt]
                , _sMaybePredicate  :: Maybe Exp
                , _sUpdates         :: [Stmt]
                , _sBody            :: [Stmt]
                }

  | WhileS      { _sPredicate       :: Exp
                , _sBody            :: [Stmt]
                }

  | DoWhileS    { _sBody            :: [Stmt]
                , _sPredicate       :: Exp
                }

  | ReturnS     { _sMaybeExp        :: Maybe Exp }

mkLabels [''Initialiser, ''ArrayExp, ''Exp, ''Declarator, ''Stmt]
