module Utils
    ( Type(..)
    , Name
    , getName
    , getPType
    , debug -- tmp
    ) where

import Data.List (intercalate)
import Debug.Trace (trace, traceStack)

import AbsGrusGrus

-- TODO TMP
debug = flip traceStack

type Name = String

getName :: TypedIdent -> Name
getName (TypedIdent (Ident ident) _) = ident

getPType :: TypedIdent -> ParserType
getPType (TypedIdent _ ptype) = ptype

data Type
    = TInt
    | TBool
    | TArrow [Type] Type
    | TAlgebraic Name
    deriving (Eq)

instance Show Type where
    show TInt = "Int"
    show TBool = "Bool"
    show (TAlgebraic name) = name
    show (TArrow leftTypes rightType) = "(" ++ intercalate ", " (map show leftTypes) ++ ") -> " ++ show rightType