module Executor
    ( ExecuteM
    , execute
    , runExecuteM
    ) where

import Control.Monad.Except (Except, runExcept)
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import AbsGrusGrus

data Value
    = VInt Integer
    | VBool Bool
    | VUnit
    | VFun Function
    deriving (Show)

type Identifier = String

data Function =
    Function Identifier Body Env
    deriving (Show)

class RuntimeExtract a where
    extract :: Value -> a

instance RuntimeExtract Integer where
    extract (VInt i) = i
    extract _ = error "TMP runtime extract int"

instance RuntimeExtract Bool where
    extract (VBool b) = b
    extract _ = error "TMP runtime extract bool"

instance RuntimeExtract Function where
    extract (VFun fun) = fun
    extract _ = error "TMP runtime extract fun"

type Loc = Int

type Env = M.Map Identifier Value

emptyEnv :: Env
emptyEnv = M.empty

lookupEnv :: Identifier -> ExecuteM (Maybe Value)
lookupEnv ident = asks (M.lookup ident)

findEnv :: String -> ExecuteM Value
findEnv ident = do
    mVal <- lookupEnv ident
    case mVal of
        Just val -> return val
        Nothing -> error "TMP variable not in env"

modifyEnv :: Identifier -> Value -> Env -> Env
modifyEnv = M.insert

type ExecuteM = ReaderT Env IO

runExecuteM :: ExecuteM a -> IO a
runExecuteM executable = runReaderT executable emptyEnv

class Executable a where
    execute :: a -> ExecuteM Value

instance Executable Body where
    execute (Body [] e) = execute e
    execute (Body (DPut exp:ds) bodyExp) = do
        v <- execute exp
        lift . putStrLn $ "put " ++ show v
        execute (Body ds bodyExp)
    execute (Body (DVal (Ident ident) _ dExp:ds) bodyExp) = do
        v <- execute dExp
        local (modifyEnv ident v) $ execute (Body ds bodyExp)
    execute (Body (DFun1 (Ident fident) (Ident pident) _ _ fbody:ds) bodyExp) = do
        env <- ask
        let fun = Function pident fbody env
        local (modifyEnv fident (VFun fun)) $ execute (Body ds bodyExp)

executeBinaryOp :: (RuntimeExtract a) => Exp -> Exp -> (a -> a -> b) -> (b -> Value) -> ExecuteM Value
executeBinaryOp e1 e2 op value = do
    v1 <- execute e1
    v2 <- execute e2
    return $ value $ extract v1 `op` extract v2

instance Executable Exp where
    execute (EIfte eb e1 e2) = do
        vb <- execute eb
        if extract vb
            then execute e1
            else execute e2
    execute (EOr e1 e2) = executeBinaryOp e1 e2 (||) VBool
    execute (EAnd e1 e2) = executeBinaryOp e1 e2 (&&) VBool
    execute (EEq e1 e2) = executeBinaryOp e1 e2 ((==) :: Integer -> Integer -> Bool) VBool
    execute (ENeq e1 e2) = executeBinaryOp e1 e2 ((/=) :: Integer -> Integer -> Bool) VBool
    execute (ELt e1 e2) = executeBinaryOp e1 e2 ((<) :: Integer -> Integer -> Bool) VBool
    execute (EGt e1 e2) = executeBinaryOp e1 e2 ((>) :: Integer -> Integer -> Bool) VBool
    execute (ELe e1 e2) = executeBinaryOp e1 e2 ((<=) :: Integer -> Integer -> Bool) VBool
    execute (EGe e1 e2) = executeBinaryOp e1 e2 ((>=) :: Integer -> Integer -> Bool) VBool
    execute (EAdd e1 e2) = executeBinaryOp e1 e2 (+) VInt
    execute (ESub e1 e2) = executeBinaryOp e1 e2 (-) VInt
    execute (EMult e1 e2) = executeBinaryOp e1 e2 (*) VInt
    execute (EDiv e1 e2) = executeBinaryOp e1 e2 div VInt
    execute (EMod e1 e2) = executeBinaryOp e1 e2 mod VInt
    execute (ENot e) = do
        v <- execute e
        return $ VBool $ not $ extract v
    execute (ECall1 (Ident fname) exp) = do
        val <- execute exp
        vfun <- findEnv fname
        let (Function pname body env) = extract vfun
        local (\_ -> modifyEnv pname val . modifyEnv fname vfun $ env) $ execute body
    execute (EInt i) = return $ VInt i
    execute (EBool BTrue) = return $ VBool True
    execute (EBool BFalse) = return $ VBool False
    execute (EUnit _) = return VUnit
    execute (EVar (Ident var)) = findEnv var