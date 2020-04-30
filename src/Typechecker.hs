module Typechecker
    ( TypecheckMonad
    , typecheck
    , runTypecheckM
    , Type
    ) where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as M

import AbsGrusGrus
import Data.List (intercalate)
import IErr
import Utils

data Env =
    Env
        { vartable :: M.Map Name Type
        }

emptyEnv :: Env
emptyEnv = Env {vartable = M.empty}

addVariableEnv :: Name -> Type -> Env -> Env
addVariableEnv vname vtype env@Env {vartable = vtable} = env {vartable = M.insert vname vtype vtable}

addVariablesEnv :: [Name] -> [Type] -> Env -> Env
addVariablesEnv vnames vtypes env = foldl (\e (n, t) -> addVariableEnv n t e) env (zip vnames vtypes)

lookupVariableEnv :: Name -> Env -> Maybe Type
lookupVariableEnv vname Env {vartable = vtable} = M.lookup vname vtable

type TypecheckMonad = ReaderT Env (Except IError)

runTypecheckM :: TypecheckMonad a -> Either IError a
runTypecheckM typecheckable = runExcept (runReaderT typecheckable emptyEnv)

class Typecheckable a where
    typecheck :: a -> TypecheckMonad Type

instance Typecheckable ParserType where
    typecheck PTInt = return TInt
    typecheck PTBool = return TBool
    typecheck (PTAlg (UIdent aname)) = return $ TAlgebraic aname
    typecheck (PTArrow leftPType rightPType) = do
        leftType <- typecheck leftPType
        rightType <- typecheck rightPType
        return $ TArrow [leftType] rightType

guardType :: Type -> Type -> TypecheckMonad ()
guardType expectedType actualType =
    when (actualType /= expectedType) $ throwError $ TypecheckError $ UnexpectedTypeError expectedType actualType `debug` "0"

guardTypecheckType :: Typecheckable a => Type -> a -> TypecheckMonad ()
guardTypecheckType expectedType typecheckable = do
    actualType <- typecheck typecheckable
    guardType expectedType actualType

typecheckBinaryOp :: Exp -> Exp -> Type -> Type -> TypecheckMonad ()
typecheckBinaryOp leftExp rightExp expectedLeftType expectedRightType = do
    guardTypecheckType expectedLeftType leftExp
    guardTypecheckType expectedRightType rightExp

-- todo Case
instance Typecheckable Exp where
    typecheck (EInt _) = return TInt
    typecheck (EBool _) = return TBool
    typecheck (EVar (Ident vname)) = do
        env <- ask
        case lookupVariableEnv vname env of
            Nothing -> throwError (TypecheckError $ VariableNotInScopeError vname)
            Just t -> return t
    typecheck (EIfte condition leftExp rightExp) = do
        guardTypecheckType TBool condition
        leftType <- typecheck leftExp
        guardTypecheckType leftType rightExp
        return leftType
    typecheck (EOr e1 e2) = typecheckBinaryOp e1 e2 TBool TBool >> return TBool
    typecheck (EAnd e1 e2) = typecheckBinaryOp e1 e2 TBool TBool >> return TBool
    typecheck (EEq e1 e2) = typecheckBinaryOp e1 e2 TInt TInt >> return TBool
    typecheck (ENeq e1 e2) = typecheckBinaryOp e1 e2 TInt TInt >> return TBool
    typecheck (ELt e1 e2) = typecheckBinaryOp e1 e2 TInt TInt >> return TBool
    typecheck (EGt e1 e2) = typecheckBinaryOp e1 e2 TInt TInt >> return TBool
    typecheck (ELe e1 e2) = typecheckBinaryOp e1 e2 TInt TInt >> return TBool
    typecheck (EGe e1 e2) = typecheckBinaryOp e1 e2 TInt TInt >> return TBool
    typecheck (EAdd e1 e2) = typecheckBinaryOp e1 e2 TInt TInt >> return TInt
    typecheck (ESub e1 e2) = typecheckBinaryOp e1 e2 TInt TInt >> return TInt
    typecheck (EMult e1 e2) = typecheckBinaryOp e1 e2 TInt TInt >> return TInt
    typecheck (EDiv e1 e2) = typecheckBinaryOp e1 e2 TInt TInt >> return TInt
    typecheck (EMod e1 e2) = typecheckBinaryOp e1 e2 TInt TInt >> return TInt
    typecheck (ECall funExp argExpressions) = do
        funType <- typecheck funExp
        case funType of
            (TArrow argExpectedTypes resultExpectedType) -> do
                let numExpectedArgs = length argExpectedTypes
                let numActualArgs = length argExpressions
                let ordSuppliedArguments = compare numExpectedArgs numActualArgs
                when (numExpectedArgs < numActualArgs) $
                    throwError $ TypecheckError $ TooManyArgumentsError numExpectedArgs numActualArgs
                mapM_ (uncurry guardTypecheckType) (zip argExpectedTypes argExpressions)
                if numExpectedArgs == numActualArgs
                    then return resultExpectedType
                    else return $ TArrow (drop numActualArgs argExpectedTypes) resultExpectedType
            _ -> throwError $ TypecheckError $ NonArrowTypeError funType
    typecheck (ELambda typedParams body) = do
        let paramNames = map getName typedParams
        let paramPTypes = map getPType typedParams
        paramTypes <- mapM typecheck paramPTypes
        bodyType <- local (addVariablesEnv paramNames paramTypes) $ typecheck body
        return $ TArrow paramTypes bodyType

--    typecheck (EAlg (TAV (UIdent algValue))) = return $ VAlg algValue []
--    typecheck (EAlg (TAVArgs (UIdent algValue) exps)) = do
--        vals <- mapM typecheck exps
--        return $ VAlg algValue vals
instance Typecheckable Body where
    typecheck (Body [] e) = typecheck e
    typecheck (Body (DPut _:ds) e) = typecheck (Body ds e)
    typecheck (Body (DVal (TypedIdent (Ident valName) expectedPType) valExp:ds) bodyExp) = do
        expectedType <- typecheck expectedPType
        guardTypecheckType expectedType valExp
        local (addVariableEnv valName expectedType) $ typecheck (Body ds bodyExp)
    typecheck (Body (DFun (Ident funName) typedParams bodyPType body:ds) bodyExp) = do
        bodyExpectedType <- typecheck bodyPType
        let paramNames = map getName typedParams
        let paramPTypes = map getPType typedParams
        paramTypes <- mapM typecheck paramPTypes
        let funType = TArrow paramTypes bodyExpectedType
        local (addVariableEnv funName funType . addVariablesEnv paramNames paramTypes) $
            guardTypecheckType bodyExpectedType body
        return $ TArrow paramTypes bodyExpectedType
--    typecheck (Body (DAlg algType algValues:ds) bodyExp) = typecheck (Body ds bodyExp)