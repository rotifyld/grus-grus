module ExecutorUtils where

import Data.List (intercalate)
import qualified Data.Map as M

import AbsGrusGrus
import Utils

data Value
    = VInt Integer
    | VBool Bool
    | VFun Function
    | VAlg String [Value]

instance Show Value where
    show (VInt int) = show int
    show (VBool bool) = show bool
    show (VFun (Function (Just name) _ _ _)) = "Function \"" ++ name ++ "\""
    show (VFun (Function Nothing _ _ _)) = "Anonymous function."
    show (VAlg name []) = name
    show (VAlg name vals) = name ++ "(" ++ (intercalate ", " . map show) vals ++ ")"

data Function =
    Function (Maybe Name) [Name] Body Env
    deriving (Show)

type Env = M.Map Name Value

emptyEnv :: Env
emptyEnv = M.empty

addEnv :: Name -> Value -> Env -> Env
addEnv = M.insert

addManyEnv :: [Name] -> [Value] -> Env -> Env
addManyEnv names values env = foldMap (uncurry addEnv) (zip names values) env

unionEnv :: Env -> Env -> Env
unionEnv = M.union