module Executor
    ( ExecuteM
    , execute
    , runExecuteM
    , Value
    , Function
    , Env
    ) where

import Control.Monad.Except
import Control.Monad.List (ListT, runListT)
import Control.Monad.Reader
import Data.List (intercalate)
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
    extract _ = throwError $ ExecutionError $ UnexpectedTypeExecutionError

instance Extractable Bool where
    extract (VBool b) = return b
    extract _ = throwError $ ExecutionError $ UnexpectedTypeExecutionError

instance Extractable Function where
    extract (VFun fun) = return fun
    extract _ = throwError $ ExecutionError $ UnexpectedTypeExecutionError

type Loc = Int

initEnv :: Env
initEnv = initialExecuteEnv

lookupEnv :: Name -> ExecuteM (Maybe Value)
lookupEnv ident = asks (M.lookup ident)

findEnv :: String -> ExecuteM Value
findEnv ident = do
    mVal <- lookupEnv ident
    case mVal of
        Just val -> return val
        Nothing -> throwError $ ExecutionError $ VariableNotInScopeExecutionError ident

type ExecuteM = ListT (ReaderT Env (ExceptT IError IO))

runExecuteM :: ExecuteM a -> IO (Either IError [a])
runExecuteM executable = runExceptT $ runReaderT (runListT executable) initEnv

class Executable a where
    execute :: a -> ExecuteM Value

instance Executable Body where
    execute (Body [] e) = execute e
    execute (Body (DPut exp:ds) bodyExp) = do
        v <- execute exp
        liftIO . putStrLn $ ">> " ++ show v
        execute (Body ds bodyExp)
    execute (Body (DVal typedIdent dExp:ds) bodyExp) = do
        v <- execute dExp
        let ident = getName typedIdent
        local (addEnv ident v) $ execute (Body ds bodyExp)
    execute (Body (DFun (LIdent fident) paramsTyped _ fbody:ds) bodyExp) = do
        env <- ask
        let params = map getName paramsTyped
        let fun = Function (Just fident) params fbody env
        local (addEnv fident (VFun fun)) $ execute (Body ds bodyExp)
    execute (Body (DAlg algType algValues:ds) bodyExp) = execute (Body ds bodyExp)

executeCaseAlternative :: [(Exp, Value)] -> Exp -> ExecuteM (Maybe Value)
executeCaseAlternative [] right = do
    val <- execute right
    return $ Just val
executeCaseAlternative ((EVar (LIdent name), value):ps) right =
    local (addEnv name value) $ executeCaseAlternative ps right
executeCaseAlternative ((EInt intE, VInt intV):ps) right
    | intE == intV = executeCaseAlternative ps right
executeCaseAlternative ((EBool BTrue, VBool True):ps) right = executeCaseAlternative ps right
executeCaseAlternative ((EBool BFalse, VBool False):ps) right = executeCaseAlternative ps right
executeCaseAlternative ((EAlg (UIdent algValE), VAlg algValV []):ps) right
    | algValE == algValV = executeCaseAlternative ps right
executeCaseAlternative ((ECall (EAlg (UIdent algE)) exps, VAlg algV vals):ps) right
    | algE == algV = executeCaseAlternative (zip exps vals ++ ps) right
executeCaseAlternative _ _ = return Nothing

executeCaseExpression :: [Case] -> Value -> ExecuteM Value
executeCaseExpression [] _ = mzero
executeCaseExpression (Case left right:cs) matchVal = do
    maybeVal <- executeCaseAlternative [(left, matchVal)] right
    case maybeVal of
        Nothing -> executeCaseExpression cs matchVal
        Just val -> return val `mplus` executeCaseExpression cs matchVal

executeOp :: (Extractable a, Extractable b) => Exp -> Exp -> (a -> b -> c) -> ExecuteM c
executeOp e1 e2 op = do
    v1 <- extract =<< execute e1
    v2 <- extract =<< execute e2
    return $ v1 `op` v2

executeFunctionCall :: Function -> [Exp] -> ExecuteM Value
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

executeAlgebraicConstructor :: Name -> [Value] -> [Exp] -> ExecuteM Value
executeAlgebraicConstructor name algVals exps = do
    newVals <- mapM execute exps
    return $ VAlg name (algVals ++ newVals)

instance Executable Exp where
    execute (EIfte eb e1 e2) = do
        vb <- extract =<< execute eb
        if vb
            then execute e1
            else execute e2
    execute (ECase exp cases) = do
        val <- execute exp
        executeCaseExpression cases val
    execute (EOr e1 e2) = do
        v1 <- extract =<< execute e1
        if v1
            then return $ VBool True
            else liftM VBool $ extract =<< execute e2
    execute (EAnd e1 e2) = do
        v1 <- extract =<< execute e1
        if not v1
            then return $ VBool False
            else liftM VBool $ extract =<< execute e2
    execute (EEq e1 e2) = liftM VBool $ executeOp e1 e2 ((==) :: Integer -> Integer -> Bool)
    execute (ENeq e1 e2) = liftM VBool $ executeOp e1 e2 ((/=) :: Integer -> Integer -> Bool)
    execute (ELt e1 e2) = liftM VBool $ executeOp e1 e2 ((<) :: Integer -> Integer -> Bool)
    execute (EGt e1 e2) = liftM VBool $ executeOp e1 e2 ((>) :: Integer -> Integer -> Bool)
    execute (ELe e1 e2) = liftM VBool $ executeOp e1 e2 ((<=) :: Integer -> Integer -> Bool)
    execute (EGe e1 e2) = liftM VBool $ executeOp e1 e2 ((>=) :: Integer -> Integer -> Bool)
    execute (EAdd e1 e2) = liftM VInt $ executeOp e1 e2 (+)
    execute (ESub e1 e2) = liftM VInt $ executeOp e1 e2 (-)
    execute (EMult e1 e2) = liftM VInt $ executeOp e1 e2 (*)
    execute (EDiv e1 e2) = do
        divisor <- extract =<< execute e2
        when (divisor == 0) $ throwError $ ExecutionError $ DivideByZeroError
        dividend <- extract =<< execute e1
        return $ VInt $ dividend `div` divisor
    execute (EMod e1 e2) = liftM VInt $ executeOp e1 e2 mod
    execute (ECall exp exps) = do
        value <- execute exp
        case value of
            (VFun fun) -> executeFunctionCall fun exps
            (VAlg name vals) -> executeAlgebraicConstructor name vals exps
            _ -> throwError $ ExecutionError $ UnexpectedTypeExecutionError
    execute (ELambda paramsTyped body) = do
        env <- ask
        let params = map getName paramsTyped
        let fun = Function Nothing params body env
        return $ VFun fun
    execute (EInt i) = return $ VInt i
    execute (EBool BTrue) = return $ VBool True
    execute (EBool BFalse) = return $ VBool False
    execute (EVar (LIdent name)) = findEnv name
    execute (EAlg (UIdent algValue)) = return $ VAlg algValue []