module Executor
    ( ExecuteM
    , execute
    , runExecuteM
    , Value
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map as M
import Debug.Trace (trace)

import AbsGrusGrus
import IErr
import Utils

-- TODO TMP
debug = flip trace

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

type Env = M.Map Name Value

emptyEnv :: Env
emptyEnv = M.empty

lookupEnv :: Name -> ExecuteM (Maybe Value)
lookupEnv ident = asks (M.lookup ident)

findEnv :: String -> ExecuteM Value
findEnv ident = do
    mVal <- lookupEnv ident
    case mVal of
        Just val -> return val
        Nothing -> error "TMP variable not in env"

modifyEnv :: Name -> Value -> Env -> Env
modifyEnv = M.insert

unionEnv :: Env -> Env -> Env
unionEnv = M.union

type ExecuteM = ReaderT Env (ExceptT IError IO)

runExecuteM :: ExecuteM a -> IO (Either IError a)
runExecuteM executable = runExceptT (runReaderT executable emptyEnv)

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
        local (modifyEnv ident v) $ execute (Body ds bodyExp)
    execute (Body (DFun (Ident fident) paramsTyped _ fbody:ds) bodyExp) = do
        env <- ask
        let params = map getName paramsTyped
        let fun = Function (Just fident) params fbody env
        local (modifyEnv fident (VFun fun)) $ execute (Body ds bodyExp)
    execute (Body (DAlg algType algValues:ds) bodyExp) = execute (Body ds bodyExp)

-- todo matchM
buildMatch :: Value -> Exp -> StateT Env Maybe ()
buildMatch val (EVar (Ident var)) = do
    modify $ modifyEnv var val
    return ()
buildMatch (VInt intV) (EInt intE)
    | intV == intE = return ()
buildMatch (VBool True) (EBool BTrue) = return ()
buildMatch (VBool False) (EBool BFalse) = return ()
buildMatch (VAlg algvalV []) (EAlg (TAV (UIdent algvalE)))
    | algvalV == algvalE = return ()
buildMatch (VAlg algV vals) (EAlg (TAVArgs (UIdent algE) exps))
    | algV == algE = mapM_ (uncurry buildMatch) (zip vals exps)
buildMatch _ _ = throwError ()

-- TODO branch exectuion
executeCase :: Value -> [Case] -> ExecuteM Value
executeCase val (Case matching body:cases) =
    case runStateT (buildMatch val matching) emptyEnv of
        Nothing -> executeCase val cases
        Just ((), env) -> local (unionEnv env) $ execute body
executeCase val [] = throwError (ExecutionError NoPatternMatcherErrorTMP)

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
                Just funIdent -> modifyEnv funIdent (VFun fun) env
    case compare (length vals) (length params) of
        GT -> error "TMP too many arguments"
        LT -> do
            let env' = foldl (\e (p, v) -> modifyEnv p v e) env0 (zip params vals)
            let leftParams = drop (length vals) params
            return $ VFun (Function mIdent leftParams body env')
        EQ -> do
            let env' = foldl (\e (p, v) -> modifyEnv p v e) env0 (zip params vals)
            local (const env') $ execute body

instance Executable Exp where
    execute (EIfte eb e1 e2) = do
        vb <- execute eb
        if extract vb
            then execute e1
            else execute e2
    execute (ECase exp cases) = do
        val <- execute exp
        executeCase val cases
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
        vfun <- execute exp
        executeFunctionCall (extract vfun) exps
    execute (ELambda paramsTyped body) = do
        env <- ask
        let params = map getName paramsTyped
        let fun = Function Nothing params body env
        return $ VFun fun
    execute (EInt i) = return $ VInt i
    execute (EBool BTrue) = return $ VBool True
    execute (EBool BFalse) = return $ VBool False
    execute (EVar (Ident name)) = findEnv name
    execute (EAlg (TAV (UIdent algValue))) = return $ VAlg algValue []
    execute (EAlg (TAVArgs (UIdent algValue) exps)) = do
        vals <- mapM execute exps
        return $ VAlg algValue vals