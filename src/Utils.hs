{-# LANGUAGE FlexibleInstances #-}

module Utils
    ( Name
    , getName
    , getNameA
    , getPType
    , getPTypes
    , Pos
    , showPos
    , noPos
    , WithPos
    , pos
    ) where

import AbsGrusGrus

type Name = String

getName :: TypedIdent a -> Name
getName (TypedIdent _ (LIdent name) _) = name

getNameA :: TypeAlgConstr a -> Name
getNameA (TAC _ (UIdent name)) = name
getNameA (TACArgs _ (UIdent name) _) = name

getPType :: TypedIdent a -> ParserType a
getPType (TypedIdent _ _ t) = t

getPTypes :: TypeAlgConstr a -> [ParserType a]
getPTypes (TAC _ _) = []
getPTypes (TACArgs _ _ types) = types

type Pos = Maybe (Int, Int)

showPos :: Pos -> String
showPos (Nothing) = "unknown position"
showPos (Just (row, col)) = "row " ++ show row ++ " column " ++ show col

noPos :: Pos
noPos = Just (-1, -1)

class WithPos a where
    pos :: a -> Pos

instance WithPos (Exp Pos) where
    pos (EIfte p _ _ _) = p
    pos (ECase p _ _) = p
    pos (EInt p _) = p
    pos (EBool p _) = p
    pos (EUnit p _) = p
    pos (EVar p _) = p
    pos (EAlg p _) = p
    pos (EOr p _ _) = p
    pos (EAnd p _ _) = p
    pos (EEq p _ _) = p
    pos (ENeq p _ _) = p
    pos (ELt p _ _) = p
    pos (EGt p _ _) = p
    pos (ELe p _ _) = p
    pos (EGe p _ _) = p
    pos (EAdd p _ _) = p
    pos (ESub p _ _) = p
    pos (EMult p _ _) = p
    pos (EDiv p _ _) = p
    pos (EMod p _ _) = p
    pos (ECall p _ _) = p
    pos (ELambda p _ _) = p

instance WithPos (Body Pos) where
    pos (Body p _ _) = p

instance WithPos (TypeAlgConstr Pos) where
    pos (TAC p _) = p
    pos (TACArgs p _ _) = p