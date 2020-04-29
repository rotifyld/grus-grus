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
import Data.Maybe (fromMaybe)

import AbsGrusGrus
import InterpreterError

data RuntimeException =
    DivideByZero

data Value
    = VInt Integer
    | VBool Bool
    | VUnit
    | VFun Function
    | VAlg String [Value]

instance Show Value where
    show (VInt int) = show int
    show (VBool bool) = show bool
    show VUnit = "Unit"
    show (VFun (Function (Just name) _ _ _)) = "Function \"" ++ name ++ "\""
    show (VFun (Function Nothing _ _ _)) = "Anonymous function"
    show (VAlg name []) = name
    show (VAlg name vals) = name ++ "(" ++ (intercalate ", " . map show) vals ++ ")"

type Identifier = String

getIdentifier :: TypedIdent -> Identifier
getIdentifier (TypedIdent (Ident ident) _) = ident

data Function =
    Function (Maybe Identifier) [Identifier] Body Env
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
        let ident = getIdentifier typedIdent
        local (modifyEnv ident v) $ execute (Body ds bodyExp)
    execute (Body (DFun (Ident fident) paramsTyped _ fbody:ds) bodyExp) = do
        env <- ask
        let params = map getIdentifier paramsTyped
        let fun = Function (Just fident) params fbody env
        local (modifyEnv fident (VFun fun)) $ execute (Body ds bodyExp)
    execute (Body (DAlg algType algValues:ds) bodyExp) = execute (Body ds bodyExp)

executeBinaryOp :: (RuntimeExtract a) => Exp -> Exp -> (a -> a -> b) -> (b -> Value) -> ExecuteM Value
executeBinaryOp e1 e2 op value = do
    v1 <- execute e1
    v2 <- execute e2
    return $ value $ extract v1 `op` extract v2

callFunction :: Function -> [Exp] -> ExecuteM Value
callFunction fun@(Function mIdent params body env) exps = do
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
            then throwError (IEError DivByZero)
            else executeBinaryOp e1 e2 div VInt
    execute (EMod e1 e2) = executeBinaryOp e1 e2 mod VInt
    execute (ECall exp exps) = do
        vfun <- execute exp
        callFunction (extract vfun) exps
    execute (ELambda paramsTyped body) = do
        env <- ask
        let params = map getIdentifier paramsTyped
        let fun = Function Nothing params body env
        return $ VFun fun
    execute (EInt i) = return $ VInt i
    execute (EBool BTrue) = return $ VBool True
    execute (EBool BFalse) = return $ VBool False
    execute (EUnit _) = return VUnit
    execute (EVar (Ident name)) = findEnv name
    execute (EAlg (TAV (UIdent algValue))) = return $ VAlg algValue []
    execute (EAlg (TAVArgs (UIdent algValue) exps)) = do
        vals <- mapM execute exps
        return $ VAlg algValue vals