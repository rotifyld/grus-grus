module TypecheckerUtils
    ( Type(..)
    , Env(..)
    , emptyEnv
    , addVariableEnv
    , addVariablesEnv
    , lookupVariableEnv
    , addConstructorEnv
    , addConstructorsEnv
    , lookupConstructorEnv
    ) where

import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S

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
        , constructorSet :: S.Set Name
        }
    deriving (Show)

emptyEnv :: Env
emptyEnv = Env {variableTable = M.empty, constructorSet = S.empty}

addVariableEnv :: Name -> Type -> Env -> Env
addVariableEnv vname vtype env@Env {variableTable = vtable} = env {variableTable = M.insert vname vtype vtable}

addVariablesEnv :: [Name] -> [Type] -> Env -> Env
addVariablesEnv vnames vtypes env = foldl (\e (n, t) -> addVariableEnv n t e) env (zip vnames vtypes)

addConstructorEnv :: Name -> Env -> Env
addConstructorEnv name env@Env {constructorSet = cset} = env {constructorSet = S.insert name cset}

addConstructorsEnv :: [Name] -> Env -> Env
addConstructorsEnv names env = foldl (flip addConstructorEnv) env names

lookupConstructorEnv :: Name -> Env -> Bool
lookupConstructorEnv name env = S.member name $ constructorSet env

lookupVariableEnv :: Name -> Env -> Maybe Type
lookupVariableEnv vname Env {variableTable = vtable} = M.lookup vname vtable