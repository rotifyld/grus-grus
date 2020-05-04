module Executor
    ( ExecuteM
    , execute
    , runExecuteM
    , Value
    , Function
    , Env
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map as M

import AbsGrusGrus
import ExecutorUtils
import IErr
import StandardLibrary (initialExecuteEnv)
import Utils

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

initEnv :: Env
initEnv = initialExecuteEnv

lookupEnv :: Name -> ExecuteM (Maybe Value)
lookupEnv ident = asks (M.lookup ident)

findEnv :: String -> ExecuteM Value
findEnv ident = do
    mVal <- lookupEnv ident
    case mVal of
        Just val -> return val
        Nothing -> error "TMP variable not in env"

type ExecuteM = ReaderT Env (ExceptT IError IO)

runExecuteM :: ExecuteM a -> IO (Either IError a)
runExecuteM executable = runExceptT (runReaderT executable initEnv)

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
    execute (Body (DFun (Ident fident) paramsTyped _ fbody:ds) bodyExp) = do
        env <- ask
        let params = map getName paramsTyped
        let fun = Function (Just fident) params fbody env
        local (addEnv fident (VFun fun)) $ execute (Body ds bodyExp)
    execute (Body (DAlg algType algValues:ds) bodyExp) = execute (Body ds bodyExp)

typecheckCaseAlternative :: [(Exp, Value)] -> Exp -> ExecuteM (Maybe Value)
typecheckCaseAlternative [] right = do
    val <- execute right
    return $ Just val
typecheckCaseAlternative ((EVar (Ident name), value):ps) right =
    local (addEnv name value) $ typecheckCaseAlternative ps right
typecheckCaseAlternative ((EInt intE, VInt intV):ps) right
    | intE == intV = typecheckCaseAlternative ps right
typecheckCaseAlternative ((EBool BTrue, VBool True):ps) right = typecheckCaseAlternative ps right
typecheckCaseAlternative ((EBool BFalse, VBool False):ps) right = typecheckCaseAlternative ps right
typecheckCaseAlternative ((EAlg (UIdent algValE), VAlg algValV []):ps) right
    | algValE == algValV = typecheckCaseAlternative ps right
typecheckCaseAlternative ((ECall (EAlg (UIdent algE)) exps, VAlg algV vals):ps) right
    | algE == algE = typecheckCaseAlternative (zip exps vals ++ ps) right
typecheckCaseAlternative _ _ = return Nothing

executeCaseExpression :: [Case] -> Value -> ExecuteM Value
executeCaseExpression [] _ = throwError $ ExecutionError NoPatternMatchedError
executeCaseExpression (Case left right:cs) matchVal = do
    maybeVal <- typecheckCaseAlternative [(left, matchVal)] right
    case maybeVal of
        Nothing -> executeCaseExpression cs matchVal
        Just val -> return val

executeBinaryOp :: (RuntimeExtract a) => Exp -> Exp -> (a -> a -> b) -> (b -> Value) -> ExecuteM Value
executeBinaryOp e1 e2 op value = do
    v1 <- execute e1
    v2 <- execute e2
    return $ value $ extract v1 `op` extract v2

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
        vb <- execute eb
        if extract vb
            then execute e1
            else execute e2
    execute (ECase exp cases) = do
        val <- execute exp
        executeCaseExpression cases val
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
            then throwError (ExecutionError DivideByZeroError)
            else executeBinaryOp e1 e2 div VInt
    execute (EMod e1 e2) = executeBinaryOp e1 e2 mod VInt
    execute (ECall exp exps) = do
        value <- execute exp
        case value of
            (VFun fun) -> executeFunctionCall fun exps
            (VAlg name vals) -> executeAlgebraicConstructor name vals exps
            _ -> error "TMP Should be found at Typechecking phase"
    execute (ELambda paramsTyped body) = do
        env <- ask
        let params = map getName paramsTyped
        let fun = Function Nothing params body env
        return $ VFun fun
    execute (EInt i) = return $ VInt i
    execute (EBool BTrue) = return $ VBool True
    execute (EBool BFalse) = return $ VBool False
    execute (EVar (Ident name)) = findEnv name
    execute (EAlg (UIdent algValue)) = return $ VAlg algValue []