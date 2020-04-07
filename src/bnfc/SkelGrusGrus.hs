module SkelGrusGrus where

-- Haskell module generated by the BNF converter

import AbsGrusGrus
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transBody :: Body -> Result
transBody x = case x of
  Body decls exp -> failure x
transDecl :: Decl -> Result
transDecl x = case x of
  DPut exp -> failure x
  DVal typedident exp -> failure x
  DFun ident typedidents type_ body -> failure x
transTypedIdent :: TypedIdent -> Result
transTypedIdent x = case x of
  TypedIdent ident type_ -> failure x
transExp :: Exp -> Result
transExp x = case x of
  EIfte exp1 exp2 exp3 -> failure x
  EOr exp1 exp2 -> failure x
  EAnd exp1 exp2 -> failure x
  EEq exp1 exp2 -> failure x
  ENeq exp1 exp2 -> failure x
  ELt exp1 exp2 -> failure x
  EGt exp1 exp2 -> failure x
  ELe exp1 exp2 -> failure x
  EGe exp1 exp2 -> failure x
  EAdd exp1 exp2 -> failure x
  ESub exp1 exp2 -> failure x
  EMult exp1 exp2 -> failure x
  EDiv exp1 exp2 -> failure x
  EMod exp1 exp2 -> failure x
  ENot exp -> failure x
  ECallIdent ident exps -> failure x
  ECallExp exp exps -> failure x
  ELambda typedidents body -> failure x
  EInt integer -> failure x
  EBool boolean -> failure x
  EUnit unit -> failure x
  EVar ident -> failure x
transBoolean :: Boolean -> Result
transBoolean x = case x of
  BTrue -> failure x
  BFalse -> failure x
transUnit :: Unit -> Result
transUnit x = case x of
  Unit -> failure x
transType :: Type -> Result
transType x = case x of
  TInt -> failure x
  TBool -> failure x
  TArrow type_1 type_2 -> failure x

