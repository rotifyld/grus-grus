module Executor
    ( ExecuteM
    , execute
    , runExecuteM
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import AbsGrusGrus

data RuntimeException =
    DivideByZero

data Value
    = VInt Integer
    | VBool Bool
    | VUnit
    | VFun Function
    deriving (Show)

type Identifier = String

getIdentifier :: TypedIdent -> Identifier
getIdentifier (TypedIdent (Ident ident) _) = ident

data Function =
    Function [Identifier] Body Env
    deriving (Show)

class RuntimeExtract a where
    extract :: Value -> a

instance RuntimeExtract Integer where
    extract (VInt i) = i
    extract _ = error "TMP runtime extract int"

instance RuntimeExtract Bool where
    extract (VBool b) = b
    extract _ = error "TMP runtime extract bool"

instance RuntimeExtract () where
    extract VUnit = ()
    extract _ = error "TMP runtime extract unit"

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
    execute (Body (DVal typedIdent dExp:ds) bodyExp) = do
        v <- execute dExp
        let ident = getIdentifier typedIdent
        local (modifyEnv ident v) $ execute (Body ds bodyExp)
    execute (Body (DFun (Ident fident) paramsTyped _ fbody:ds) bodyExp) = do
        env <- ask
        let params = map getIdentifier paramsTyped
        let fun = Function params fbody env
        local (modifyEnv fident (VFun fun)) $ execute (Body ds bodyExp)

executeBinaryOp :: (RuntimeExtract a) => Exp -> Exp -> (a -> a -> b) -> (b -> Value) -> ExecuteM Value
executeBinaryOp e1 e2 op value = do
    v1 <- execute e1
    v2 <- execute e2
    return $ value $ extract v1 `op` extract v2

callFunction :: Maybe Identifier -> Function -> [Exp] -> ExecuteM Value
callFunction mIdent fun@(Function params body env) exps = do
    vals <- mapM execute exps
    let env0 =
            case mIdent of
                Nothing -> env
                Just funIdent -> modifyEnv funIdent (VFun fun) env
    case compare (length vals) (length params) of
        GT -> error "TMP too many arguments"
        LT -> do
            let env' = foldl (\e (p, v) -> modifyEnv p v e) env0 (zip params vals)
            let leftParams = drop (length vals) params
            return $ VFun (Function leftParams body env')
        EQ -> do
            let env' = foldl (\e (p, v) -> modifyEnv p v e) env0 (zip params vals)
            local (const env') $ execute body

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
    execute (EDiv e1 e2) = do
        v2 <- execute e2
        if (extract v2 :: Integer) == 0
            then error "TMP div zero error"
            else executeBinaryOp e1 e2 div VInt
    execute (EMod e1 e2) = executeBinaryOp e1 e2 mod VInt
    execute (ENot e) = do
        v <- execute e
        return $ VBool $ not $ extract v
    execute (ECallIdent (Ident fname) exps) = do
        vfun <- findEnv fname
        callFunction (Just fname) (extract vfun) exps
    execute (ECallExp exp exps) = do
        vfun <- execute exp
        callFunction Nothing (extract vfun) exps
    execute (ELambda paramsTyped body) = do
        env <- ask
        let params = map getIdentifier paramsTyped
        let fun = Function params body env
        return $ VFun fun
    execute (EInt i) = return $ VInt i
    execute (EBool BTrue) = return $ VBool True
    execute (EBool BFalse) = return $ VBool False
    execute (EUnit _) = return VUnit
    execute (EVar (Ident var)) = findEnv var