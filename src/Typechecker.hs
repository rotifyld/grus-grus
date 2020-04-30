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

guardType :: Typecheckable a => Type -> a -> TypecheckMonad ()
guardType expectedType typecheckable = do
    actualType <- typecheck typecheckable
    when (actualType /= expectedType) $ throwError $ TypecheckError $ UnexpectedTypeError expectedType actualType

typecheckBinaryOp :: Exp -> Exp -> Type -> Type -> TypecheckMonad ()
typecheckBinaryOp leftExp rightExp expectedLeftType expectedRightType = do
    guardType expectedLeftType leftExp
    guardType expectedRightType rightExp

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
        guardType TBool condition
        leftType <- typecheck leftExp
        guardType leftType rightExp
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
                mapM_ (uncurry guardType) (zip argExpectedTypes argExpressions)
                if numExpectedArgs == numActualArgs
                    then return resultExpectedType
                    else return $ TArrow (drop numActualArgs argExpectedTypes) resultExpectedType
            _ -> throwError $ TypecheckError $ NonArrowTypeError funType
    typecheck (ELambda typedParams body) = do
        env <- ask
        let names = map getName typedParams
        let ptypes = map getPType typedParams
        types <- mapM typecheck ptypes
        bodyType <- local (addVariablesEnv names types) $ typecheck body
        return $ TArrow types bodyType

--    typecheck (EAlg (TAV (UIdent algValue))) = return $ VAlg algValue []
--    typecheck (EAlg (TAVArgs (UIdent algValue) exps)) = do
--        vals <- mapM typecheck exps
--        return $ VAlg algValue vals
instance Typecheckable Body where
    typecheck (Body [] e) = typecheck e
    typecheck (Body (DPut _:ds) e) = typecheck (Body ds e)
    typecheck (Body (DVal (TypedIdent (Ident valName) expectedPType) valExp:ds) bodyExp) = do
        expectedType <- typecheck expectedPType
        guardType expectedType valExp
        local (addVariableEnv valName expectedType) $ typecheck (Body ds bodyExp)
--    typecheck (Body (DFun (Ident fident) paramsTyped _ fbody:ds) bodyExp) = do
--        env <- ask
--        let params = map getIdentifier paramsTyped
--        let fun = Function (Just fident) params fbody env
--        local (modifyEnv fident (VFun fun)) $ typecheck (Body ds bodyExp)
--    typecheck (Body (DAlg algType algValues:ds) bodyExp) = typecheck (Body ds bodyExp)