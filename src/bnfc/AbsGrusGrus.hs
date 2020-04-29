-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

module AbsGrusGrus where

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Read)

newtype UIdent = UIdent String
  deriving (Eq, Ord, Show, Read)

data Body = Body [Decl] Exp
  deriving (Eq, Ord, Show, Read)

data Decl
    = DPut Exp
    | DVal TypedIdent Exp
    | DFun Ident [TypedIdent] Type Body
    | DAlg UIdent [TypeAlgConstr]
  deriving (Eq, Ord, Show, Read)

data TypedIdent = TypedIdent Ident Type
  deriving (Eq, Ord, Show, Read)

data Exp
    = EIfte Exp Exp Exp
    | EOr Exp Exp
    | EAnd Exp Exp
    | EEq Exp Exp
    | ENeq Exp Exp
    | ELt Exp Exp
    | EGt Exp Exp
    | ELe Exp Exp
    | EGe Exp Exp
    | EAdd Exp Exp
    | ESub Exp Exp
    | EMult Exp Exp
    | EDiv Exp Exp
    | EMod Exp Exp
    | ECall Exp [Exp]
    | ELambda [TypedIdent] Body
    | EInt Integer
    | EBool Boolean
    | EUnit Unit
    | EVar Ident
    | EAlg TypeAlgValue
  deriving (Eq, Ord, Show, Read)

data Boolean = BTrue | BFalse
  deriving (Eq, Ord, Show, Read)

data Unit = Unit
  deriving (Eq, Ord, Show, Read)

data Type = TArrow Type Type | TInt | TAlg UIdent
  deriving (Eq, Ord, Show, Read)

data TypeAlgValue = TAV UIdent | TAVArgs UIdent [Exp]
  deriving (Eq, Ord, Show, Read)

data TypeAlgConstr = TAC UIdent | TACArgs UIdent [Type]
  deriving (Eq, Ord, Show, Read)

