{-# LANGUAGE FlexibleInstances #-}

module Executor
    ( ExecuteM
    , execute
    , runExecuteM
    , Value
    , Function
    , Env
    ) where

import Control.Monad (liftM, mplus, mzero, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.List (ListT, runListT)
import Control.Monad.Reader (ReaderT, ask, asks, local, runReaderT)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M

import AbsGrusGrus

import ExecutorUtils
import IErr
import StandardLibrary (initialExecuteEnv)
import Utils

class Extractable a where
    extract :: Value -> ExecuteM a

instance Extractable Integer where
    extract (VInt i) = return i
    extract _ = throwError $ ExecutionError UnexpectedTypeExecutionError Nothing

instance Extractable Bool where
    extract (VBool b) = return b
    extract _ = throwError $ ExecutionError UnexpectedTypeExecutionError Nothing

instance Extractable Function where
    extract (VFun fun) = return fun
    extract _ = throwError $ ExecutionError UnexpectedTypeExecutionError Nothing

type ExecuteM = ListT (ReaderT Env (ExceptT IError IO))

runExecuteM :: ExecuteM a -> IO (Either IError [a])
runExecuteM executable = runExceptT $ runReaderT (runListT executable) initEnv

initEnv :: Env
initEnv = initialExecuteEnv

lookupEnv :: Name -> ExecuteM (Maybe Value)
lookupEnv ident = asks (M.lookup ident)

findEnv :: String -> Pos -> ExecuteM Value
findEnv ident p = do
    mVal <- lookupEnv ident
    case mVal of
        Just val -> return val
        Nothing -> throwError $ ExecutionError (VariableNotInScopeExecutionError ident) p

class Executable a where
    execute :: a -> ExecuteM Value

executeCaseAlternative :: [(Exp Pos, Value)] -> Exp Pos -> ExecuteM (Maybe Value)
executeCaseAlternative [] right = do
    val <- execute right
    return $ Just val
executeCaseAlternative ((EVar _ (LIdent name), value):ps) right =
    local (addEnv name value) $ executeCaseAlternative ps right
executeCaseAlternative ((EInt _ intE, VInt intV):ps) right
    | intE == intV = executeCaseAlternative ps right
executeCaseAlternative ((EBool _ (BTrue _), VBool True):ps) right = executeCaseAlternative ps right
executeCaseAlternative ((EBool _ (BFalse _), VBool False):ps) right = executeCaseAlternative ps right
executeCaseAlternative ((EAlg _ (UIdent algValE), VAlg algValV []):ps) right
    | algValE == algValV = executeCaseAlternative ps right
executeCaseAlternative ((ECall _ (EAlg _ (UIdent algE)) exps, VAlg algV vals):ps) right
    | algE == algV = executeCaseAlternative (zip exps vals ++ ps) right
executeCaseAlternative _ _ = return Nothing

executeCaseExpression :: [Case Pos] -> Value -> ExecuteM Value
executeCaseExpression [] _ = mzero
executeCaseExpression (Case _ left right:cs) matchVal = do
    maybeVal <- executeCaseAlternative [(left, matchVal)] right
    case maybeVal of
        Nothing -> executeCaseExpression cs matchVal
        Just val -> return val `mplus` executeCaseExpression cs matchVal

executeOp :: (Extractable a, Extractable b) => Exp Pos -> Exp Pos -> (a -> b -> c) -> ExecuteM c
executeOp e1 e2 op = do
    v1 <- extract =<< execute e1
    v2 <- extract =<< execute e2
    return $ v1 `op` v2

executeFunctionCall :: Function -> [Exp Pos] -> ExecuteM Value
executeFunctionCall fun@(Function mIdent params body env) exps = do
    vals <- mapM execute exps
    let env0 =
            case mIdent of
                Nothing -> env
                Just funIdent -> addEnv funIdent (VFun fun) env
    if length vals < length params
        then do
            let env' = foldl (\e (p, v) -> addEnv p v e) env0 (zip params vals)
            let leftParams = drop (length vals) params
            return $ VFun (Function mIdent leftParams body env')
        else do
            let env' = foldl (\e (p, v) -> addEnv p v e) env0 (zip params vals)
            local (const env') $ execute body

executeAlgebraicConstructor :: Name -> [Value] -> [Exp Pos] -> ExecuteM Value
executeAlgebraicConstructor name algVals exps = do
    newVals <- mapM execute exps
    return $ VAlg name (algVals ++ newVals)

instance Executable (Exp Pos) where
    execute (EInt _ i) = return $ VInt i
    execute (EBool _ (BTrue _)) = return $ VBool True
    execute (EBool _ (BFalse _)) = return $ VBool False
    execute (EUnit _ _) = return VUnit
    execute (EVar p (LIdent name)) = findEnv name p
    execute (EAlg _ (UIdent algValue)) = return $ VAlg algValue []
    execute (EEq _ e1 e2) = liftM VBool $ executeOp e1 e2 ((==) :: Integer -> Integer -> Bool)
    execute (ENeq _ e1 e2) = liftM VBool $ executeOp e1 e2 ((/=) :: Integer -> Integer -> Bool)
    execute (ELt _ e1 e2) = liftM VBool $ executeOp e1 e2 ((<) :: Integer -> Integer -> Bool)
    execute (EGt _ e1 e2) = liftM VBool $ executeOp e1 e2 ((>) :: Integer -> Integer -> Bool)
    execute (ELe _ e1 e2) = liftM VBool $ executeOp e1 e2 ((<=) :: Integer -> Integer -> Bool)
    execute (EGe _ e1 e2) = liftM VBool $ executeOp e1 e2 ((>=) :: Integer -> Integer -> Bool)
    execute (EAdd _ e1 e2) = liftM VInt $ executeOp e1 e2 (+)
    execute (ESub _ e1 e2) = liftM VInt $ executeOp e1 e2 (-)
    execute (EMult _ e1 e2) = liftM VInt $ executeOp e1 e2 (*)
    execute (EDiv p e1 e2) = do
        divisor <- extract =<< execute e2
        when (divisor == 0) $ throwError $ ExecutionError DivideByZeroError p
        dividend <- extract =<< execute e1
        return $ VInt $ dividend `div` divisor
    execute (EMod _ e1 e2) = liftM VInt $ executeOp e1 e2 mod
    execute (EOr _ e1 e2) = do
        v1 <- extract =<< execute e1
        if v1
            then return $ VBool True
            else liftM VBool $ extract =<< execute e2
    execute (EAnd _ e1 e2) = do
        v1 <- extract =<< execute e1
        if not v1
            then return $ VBool False
            else liftM VBool $ extract =<< execute e2
    execute (EIfte _ eb e1 e2) = do
        vb <- extract =<< execute eb
        if vb
            then execute e1
            else execute e2
    execute (ECase _ expr cases) = do
        val <- execute expr
        executeCaseExpression cases val
    execute (ELambda _ paramsTyped body) = do
        env <- ask
        let params = map getName paramsTyped
        let fun = Function Nothing params body env
        return $ VFun fun
    execute (ECall p expr exps) = do
        value <- execute expr
        case value of
            (VFun fun) -> executeFunctionCall fun exps
            (VAlg name vals) -> executeAlgebraicConstructor name vals exps
            _ -> throwError $ ExecutionError UnexpectedTypeExecutionError p

instance Executable (Body Pos) where
    execute (Body _ [] e) = execute e
    execute (Body p (DPut _ expr:ds) bodyExp) = do
        v <- execute expr
        liftIO . putStrLn $ ">> " ++ show v
        execute (Body p ds bodyExp)
    execute (Body p (DVal _ typedIdent dExp:ds) bodyExp) = do
        v <- execute dExp
        let ident = getName typedIdent
        local (addEnv ident v) $ execute (Body p ds bodyExp)
    execute (Body p (DFun _ (LIdent fident) paramsTyped _ fbody:ds) bodyExp) = do
        env <- ask
        let params = map getName paramsTyped
        let fun = Function (Just fident) params fbody env
        local (addEnv fident (VFun fun)) $ execute (Body p ds bodyExp)
    execute (Body p (DAlg _ _ _:ds) bodyExp) = execute (Body p ds bodyExp)