module Typechecker
    ( TypecheckM
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
import StandardLibrary (initialTypecheckEnv)
import TypecheckerUtils
import Utils

initEnv :: Env
initEnv = initialTypecheckEnv

type TypecheckM = ReaderT Env (Except IError)

runTypecheckM :: TypecheckM a -> Either IError a
runTypecheckM typecheckable = runExcept (runReaderT typecheckable initEnv)

class Typecheckable a where
    typecheck :: a -> TypecheckM Type

instance Typecheckable ParserType where
    typecheck PTInt = return TInt
    typecheck PTBool = return TBool
    typecheck (PTAlg (UIdent aname)) = return $ TAlgebraic aname
    typecheck (PTArrow leftPType rightPType) = do
        leftType <- typecheck leftPType
        rightType <- typecheck rightPType
        return $ TArrow [leftType] rightType
    typecheck (PTArrowMult leftHeadPType leftTailPType rightPType) = do
        leftHeadType <- typecheck leftHeadPType
        leftTailType <- mapM typecheck leftTailPType
        rightType <- typecheck rightPType
        return $ TArrow (leftHeadType:leftTailType) rightType

guardType :: Type -> Type -> TypecheckM ()
guardType expectedType actualType =
    when (actualType /= expectedType) $ throwError $ TypecheckError $ UnexpectedTypeError expectedType actualType

guardBinaryType :: Exp -> Exp -> Type -> Type -> TypecheckM ()
guardBinaryType leftExp rightExp expectedLeftType expectedRightType = do
    guardWithTypecheck expectedLeftType leftExp
    guardWithTypecheck expectedRightType rightExp

guardWithTypecheck :: Typecheckable a => Type -> a -> TypecheckM ()
guardWithTypecheck expectedType typecheckable = do
    actualType <- typecheck typecheckable
    guardType expectedType actualType

typecheckManyWithGuard :: Typecheckable a => [a] -> TypecheckM Type
typecheckManyWithGuard typeable = do
    types <- mapM typecheck typeable
    case types of
        [] -> throwError $ TypecheckError EmptyCaseAlternativesListError
        (t:ts) -> mapM_ (guardType t) ts >> return t

-- Nomenclature:
--  [[case expression]] ::= case [[match]] of { [[alternative]]; ... }
--
--  where [[alternative]] ::= [[left]] ~> [[right]]
--
typecheckCaseAlternative :: [(Exp, Type)] -> Exp -> TypecheckM Type
typecheckCaseAlternative [] right = typecheck right
typecheckCaseAlternative ((EVar (LIdent vname), expectedType):es) right =
    local (addVariableEnv vname expectedType) $ typecheckCaseAlternative es right
typecheckCaseAlternative ((EInt _, TInt):es) right = typecheckCaseAlternative es right
typecheckCaseAlternative ((EBool _, TBool):es) right = typecheckCaseAlternative es right
typecheckCaseAlternative ((algVal@(EAlg _), algType@(TAlgebraic _)):es) right = do
    guardWithTypecheck algType algVal
    typecheckCaseAlternative es right
typecheckCaseAlternative ((ECall algVal@(EAlg (UIdent algValName)) callExps, algExpectedType@(TAlgebraic _)):es) right = do
    env <- ask
    case lookupVariableEnv algValName env of
        Nothing -> throwError $ TypecheckError $ AlgebraicNotInScopeError algValName
        (Just (TArrow constructorShape algActualType)) -> do
            guardType algExpectedType algActualType
            let expectedArgsNum = length constructorShape
            let actualArgsNum = length callExps
            when (expectedArgsNum /= actualArgsNum) $
                throwError $ TypecheckError $ ConstructorArgumentsError expectedArgsNum actualArgsNum
            typecheckCaseAlternative (zip callExps constructorShape ++ es) right
        (Just t) -> throwError $ TypecheckError $ UnexpectedTypeError algExpectedType t
typecheckCaseAlternative ((e, t):es) _ = throwError $ TypecheckError $ CaseTypeMismatchError t e

-- returns type of right of (any) alternative
typecheckCaseExpression :: [Case] -> Type -> TypecheckM Type
typecheckCaseExpression [] _ = throwError $ TypecheckError EmptyCaseAlternativesListError
typecheckCaseExpression [Case left right] matchType = typecheckCaseAlternative [(left, matchType)] right
typecheckCaseExpression (Case left right:cs) matchType = do
    nextAlternativesType <- typecheckCaseExpression cs matchType
    thisAlternativeType <- typecheckCaseAlternative [(left, matchType)] right
    guardType thisAlternativeType nextAlternativesType
    return thisAlternativeType

instance Typecheckable Exp where
    typecheck (EIfte condition leftExp rightExp) = do
        guardWithTypecheck TBool condition
        leftType <- typecheck leftExp
        guardWithTypecheck leftType rightExp
        return leftType
    typecheck (ECase match alternatives) = do
        matchType <- typecheck match
        typecheckCaseExpression alternatives matchType
    typecheck (EInt _) = return TInt
    typecheck (EBool _) = return TBool
    typecheck (EUnit _) = return TUnit
    typecheck (EVar (LIdent vname)) = do
        env <- ask
        case lookupVariableEnv vname env of
            Nothing -> throwError (TypecheckError $ VariableNotInScopeError vname)
            Just t -> return t
    typecheck (EAlg (UIdent aname)) = do
        env <- ask
        case lookupVariableEnv aname env of
            Nothing -> throwError (TypecheckError $ AlgebraicNotInScopeError aname)
            Just t -> return t
    typecheck (EOr e1 e2) = guardBinaryType e1 e2 TBool TBool >> return TBool
    typecheck (EAnd e1 e2) = guardBinaryType e1 e2 TBool TBool >> return TBool
    typecheck (EEq e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TBool
    typecheck (ENeq e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TBool
    typecheck (ELt e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TBool
    typecheck (EGt e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TBool
    typecheck (ELe e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TBool
    typecheck (EGe e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TBool
    typecheck (EAdd e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TInt
    typecheck (ESub e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TInt
    typecheck (EMult e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TInt
    typecheck (EDiv e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TInt
    typecheck (EMod e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TInt
    typecheck (ECall funExp argExpressions) = do
        funType <- typecheck funExp
        case funType of
            (TArrow argExpectedTypes resultExpectedType) -> do
                let numExpectedArgs = length argExpectedTypes
                let numActualArgs = length argExpressions
                let ordSuppliedArguments = compare numExpectedArgs numActualArgs
                when (numExpectedArgs < numActualArgs) $
                    throwError $ TypecheckError $ TooManyArgumentsError numExpectedArgs numActualArgs
                mapM_ (uncurry guardWithTypecheck) (zip argExpectedTypes argExpressions)
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

instance Typecheckable Body where
    typecheck (Body [] e) = typecheck e
    typecheck (Body (DPut exp:ds) e) = do
        typecheck exp
        typecheck (Body ds e)
    typecheck (Body (DVal (TypedIdent (LIdent valName) expectedPType) valExp:ds) bodyExp) = do
        expectedType <- typecheck expectedPType
        guardWithTypecheck expectedType valExp
        local (addVariableEnv valName expectedType) $ typecheck (Body ds bodyExp)
    typecheck (Body (DFun (LIdent funName) typedParams bodyPType body:ds) bodyExp) = do
        bodyExpectedType <- typecheck bodyPType
        let paramNames = map getName typedParams
        let paramPTypes = map getPType typedParams
        paramTypes <- mapM typecheck paramPTypes
        let funType = TArrow paramTypes bodyExpectedType
        local (addVariableEnv funName funType . addVariablesEnv paramNames paramTypes) $
            guardWithTypecheck bodyExpectedType body
        local (addVariableEnv funName funType) $ typecheck (Body ds bodyExp)
    typecheck (Body (DAlg (UIdent typeName) values:ds) bodyExp) = do
        let algType = TAlgebraic typeName
        let valueNames = map getName values
        let valueParamPTypes = map getPTypes values
        valueParamTypes <- mapM (mapM typecheck) valueParamPTypes
        let valueTypes =
                map
                    (\types ->
                         if null types
                             then algType
                             else TArrow types algType)
                    valueParamTypes
        local (addVariablesEnv valueNames valueTypes . addAlgShapesEnv valueNames valueParamTypes) $
            typecheck (Body ds bodyExp)