module TypecheckerUtils
    ( Type(..)
    , Env(..)
    , emptyEnv
    , addVariableEnv
    , addVariablesEnv
    , lookupVariableEnv
    ) where

import Data.List (intercalate)
import qualified Data.Map as M

import Utils

data Type
    = TInt
    | TBool
    | TUnit
    | TArrow [Type] Type
    | TAlgebraic Name
    deriving (Eq)

instance Show Type where
    show TInt = "Int"
    show TBool = "Bool"
    show TUnit = "Unit"
    show (TAlgebraic name) = name
    show (TArrow leftTypes rightType) = "(" ++ intercalate ", " (map show leftTypes) ++ ") -> " ++ show rightType

data Env =
    Env
        { variableTable :: M.Map Name Type
        }
    deriving (Show)

emptyEnv :: Env
emptyEnv = Env {variableTable = M.empty}

addVariableEnv :: Name -> Type -> Env -> Env
addVariableEnv vname vtype env@Env {variableTable = vtable} = env {variableTable = M.insert vname vtype vtable}

addVariablesEnv :: [Name] -> [Type] -> Env -> Env
addVariablesEnv vnames vtypes env = foldl (\e (n, t) -> addVariableEnv n t e) env (zip vnames vtypes)

lookupVariableEnv :: Name -> Env -> Maybe Type
lookupVariableEnv vname Env {variableTable = vtable} = M.lookup vname vtable