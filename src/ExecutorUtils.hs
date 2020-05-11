module ExecutorUtils
    ( Value(..)
    , Function(..)
    , Env
    , emptyEnv
    , addEnv
    , addManyEnv
    , unionEnv
    ) where

import Data.List (intercalate)
import qualified Data.Map as M

import AbsGrusGrus

import Utils

data Value
    = VInt Integer
    | VBool Bool
    | VUnit
    | VFun Function
    | VAlg String [Value]
    deriving (Eq)

instance Show Value where
    show (VInt int) = show int
    show (VBool bool) = show bool
    show (VUnit) = "Unit"
    show (VFun (Function (Just name) _ _ _)) = "Function \"" ++ name ++ "\""
    show (VFun (Function Nothing _ _ _)) = "Anonymous function."
    show (VAlg name []) = name
    show (VAlg name vals) = name ++ "(" ++ (intercalate ", " . map show) vals ++ ")"

data Function =
    Function (Maybe Name) [Name] (Body Pos) Env
    deriving (Show, Eq)

type Env = M.Map Name Value

emptyEnv :: Env
emptyEnv = M.empty

addEnv :: Name -> Value -> Env -> Env
addEnv = M.insert

addManyEnv :: [Name] -> [Value] -> Env -> Env
addManyEnv names values env = foldMap (uncurry addEnv) (zip names values) env

unionEnv :: Env -> Env -> Env
unionEnv = M.union