module TypecheckerUtils where

import qualified Data.Map as M

import Data.List (intercalate)
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
        , algShapeTable :: M.Map Name [Type]
        }
    deriving (Show)

emptyEnv :: Env
emptyEnv = Env {variableTable = M.empty, algShapeTable = M.empty}

addVariableEnv :: Name -> Type -> Env -> Env
addVariableEnv vname vtype env@Env {variableTable = vtable} = env {variableTable = M.insert vname vtype vtable}

addVariablesEnv :: [Name] -> [Type] -> Env -> Env
addVariablesEnv vnames vtypes env = foldl (\e (n, t) -> addVariableEnv n t e) env (zip vnames vtypes)

lookupVariableEnv :: Name -> Env -> Maybe Type
lookupVariableEnv vname Env {variableTable = vtable} = M.lookup vname vtable

addAlgShapeEnv :: Name -> [Type] -> Env -> Env
addAlgShapeEnv name shape env@Env {algShapeTable = astable} = env {algShapeTable = M.insert name shape astable}

addAlgShapesEnv :: [Name] -> [[Type]] -> Env -> Env
addAlgShapesEnv names shapes env = foldl (\e (n, s) -> addAlgShapeEnv n s e) env (zip names shapes)

lookupAlgShapeEnv :: Name -> Env -> Maybe [Type]
lookupAlgShapeEnv name Env {algShapeTable = astable} = M.lookup name astable