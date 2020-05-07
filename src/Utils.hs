module Utils
    ( Name
    , getName
    , getPType
    , getPTypes
    , debug -- tmp
    ) where

import Data.List (intercalate)
import Debug.Trace (trace, traceStack)

import AbsGrusGrus

-- TODO TMP
debug = flip traceStack

type Name = String

class WithName a where
    getName :: a -> Name

instance WithName TypedIdent where
    getName (TypedIdent (LIdent name) _) = name

instance WithName TypeAlgConstr where
    getName (TAC (UIdent name)) = name
    getName (TACArgs (UIdent name) _) = name

class WithType a where
    getPType :: a -> ParserType
    getPTypes :: a -> [ParserType]

instance WithType TypedIdent where
    getPType (TypedIdent _ t) = t
    getPTypes _ = error ""

instance WithType TypeAlgConstr where
    getPType _ = error ""
    getPTypes (TAC _) = []
    getPTypes (TACArgs _ types) = types