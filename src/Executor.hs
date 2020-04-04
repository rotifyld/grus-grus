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
    | VClosure Function
    deriving (Show)

data Function =
    Function String Body Env
    deriving (Show)

class RuntimeExtract a where
    extract :: Value -> a

instance RuntimeExtract Integer where
    extract (VInt i) = i
    extract _ = error "TMP runtime extract int"

instance RuntimeExtract Bool where
    extract (VBool b) = b
    extract _ = error "TMP runtime extract bool"

type Loc = Int

data Env =
    Env
        { envVarLocs :: M.Map String Loc
        , envFuns :: M.Map String Function
        }
    deriving (Show)

emptyEnv :: Env
emptyEnv = Env M.empty M.empty

lookupVarEnv :: String -> ExecuteM (Maybe Loc)
lookupVarEnv var = asks (M.lookup var . envVarLocs)

fromVarEnv :: String -> ExecuteM Loc
fromVarEnv var = do
    mLoc <- lookupVarEnv var
    case mLoc of
        Just loc -> return loc
        Nothing -> error "TMP variable not in env"

lookupFunEnv :: String -> ExecuteM (Maybe Function)
lookupFunEnv fname = asks (M.lookup fname . envFuns)

fromFunEnv :: String -> ExecuteM Loc
fromFunEnv var = do
    mLoc <- lookupVarEnv var
    case mLoc of
        Just loc -> return loc
        Nothing -> error "TMP variable not in env"

insertVarEnv :: String -> Loc -> Env -> Env
insertVarEnv var loc env = env {envVarLocs = M.insert var loc (envVarLocs env)}

insertFunEnv :: String -> Function -> Env -> Env
insertFunEnv fname fun env = env {envFuns = M.insert fname fun (envFuns env)}

data Store =
    Store
        { storeMap :: M.Map Loc Value
        , storeNext :: Loc
        }

emptyStore :: Store
emptyStore = Store {storeMap = M.empty, storeNext = 0}

lookupStore :: Loc -> ExecuteM (Maybe Value)
lookupStore loc = gets (M.lookup loc . storeMap)

fromStore :: Loc -> ExecuteM Value
fromStore loc = do
    mVal <- lookupStore loc
    case mVal of
        Just val -> return val
        Nothing -> error "TMP location not in store"

insertStore :: Loc -> Value -> ExecuteM ()
insertStore loc val = modify (\s -> s {storeMap = M.insert loc val (storeMap s)})

allocStore :: ExecuteM Loc
allocStore = do
    newLoc <- gets storeNext
    modify (\s -> s {storeNext = newLoc + 1})
    return newLoc

type ExecuteM = ReaderT Env (State Store)

runExecuteM :: ExecuteM a -> a
runExecuteM executable = evalState (runReaderT executable emptyEnv) emptyStore

class Executable a where
    execute :: a -> ExecuteM Value

instance Executable Body where
    execute (Body [] e) = execute e
    execute (Body (DVal (Ident var) _ dExp:ds) bExp) = do
        v <- execute dExp
        loc <- allocStore
        local (insertVarEnv var loc) $ execute (Body ds bExp)
    execute (Body (DFun1 (Ident fname) (Ident pname) _ _ fbody:ds) bExp) = do
        env <- ask
        let fun = Function pname fbody env
        local (insertFunEnv fname fun) $ execute (Body ds bExp)

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
--    execute (ECall1 (Ident fname) exp) = do
--        newLoc <- allocStore
--        val <- execute exp
--        todo
    execute (EInt i) = return $ VInt i
    execute (EBool BTrue) = return $ VBool True
    execute (EBool BFalse) = return $ VBool False
    execute (EUnit _) = return VUnit
    execute (EVar (Ident var)) = fromStore =<< fromVarEnv var