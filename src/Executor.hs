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
    | VClosure String Body Env
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

type Env = M.Map String Loc

emptyEnv :: Env
emptyEnv = M.empty

lookupEnv :: String -> ExecuteM (Maybe Loc)
lookupEnv var = asks (M.lookup var)

fromEnv :: String -> ExecuteM Loc
fromEnv var = do
    mLoc <- lookupEnv var
    case mLoc of
        Just loc -> return loc
        Nothing -> error "TMP variable not in env"

insertEnv :: String -> Loc -> Env -> Env
insertEnv = M.insert

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
        mLoc <- lookupEnv var
        loc <-
            case mLoc of
                Just l -> return l
                Nothing -> allocStore
        insertStore loc v
        local (M.insert var loc) $ execute (Body ds bExp)

--    interpret (Body (DFun1 (Ident funName) (Ident varName) _ _ body:ds) bExp) = do
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
    execute (EInt i) = return $ VInt i
    execute (EBool BTrue) = return $ VBool True
    execute (EBool BFalse) = return $ VBool False
    execute (EUnit _) = return VUnit
    execute (EVar (Ident var)) = fromStore =<< fromEnv var