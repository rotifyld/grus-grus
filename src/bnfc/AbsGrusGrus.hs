-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

module AbsGrusGrus where

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Read)

data Body = Body [Decl] Exp
  deriving (Eq, Ord, Show, Read)

data Decl = DVal Ident Type Exp | DFun1 Ident Ident Type Type Body
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
    | ECall1 Ident Exp
    | ENot Exp
    | EInt Integer
    | EBool Boolean
    | EUnit Unit
    | EVar Ident
  deriving (Eq, Ord, Show, Read)

data Boolean = BTrue | BFalse
  deriving (Eq, Ord, Show, Read)

data Unit = Unit
  deriving (Eq, Ord, Show, Read)

data Type = TInt | TBool | TArrow Type Type
  deriving (Eq, Ord, Show, Read)

