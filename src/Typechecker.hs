{-# LANGUAGE FlexibleInstances #-}

module Typechecker
    ( TypecheckM
    , typecheck
    , runTypecheckM
    , Type
    ) where

import Control.Monad (when)
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)

import AbsGrusGrus

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

guardType :: Pos -> Type -> Type -> TypecheckM ()
guardType p expectedType actualType =
    when (actualType /= expectedType) $ throwError $ TypecheckError (UnexpectedTypeError expectedType actualType) p

guardBinaryType :: Exp Pos -> Exp Pos -> Type -> Type -> TypecheckM ()
guardBinaryType leftExp rightExp expectedLeftType expectedRightType = do
    guardWithTypecheck expectedLeftType leftExp
    guardWithTypecheck expectedRightType rightExp

guardWithTypecheck :: (Typecheckable a, WithPos a) => Type -> a -> TypecheckM ()
guardWithTypecheck expectedType typecheckable = do
    actualType <- typecheck typecheckable
    guardType (pos typecheckable) expectedType actualType

instance Typecheckable (ParserType Pos) where
    typecheck (PTInt _) = return TInt
    typecheck (PTBool _) = return TBool
    typecheck (PTAlg _ (UIdent aname)) = return $ TAlgebraic aname
    typecheck (PTArrow _ leftPType rightPType) = do
        leftType <- typecheck leftPType
        rightType <- typecheck rightPType
        return $ TArrow [leftType] rightType
    typecheck (PTArrowMult _ leftHeadPType leftTailPType rightPType) = do
        leftHeadType <- typecheck leftHeadPType
        leftTailType <- mapM typecheck leftTailPType
        rightType <- typecheck rightPType
        return $ TArrow (leftHeadType : leftTailType) rightType

-- Nomenclature:
--  [[case expression]] ::= case [[match]] of { [[alternative]]; ... }
--
--  where [[alternative]] ::= [[left]] ~> [[right]]
--
typecheckCaseAlternative :: [(Exp Pos, Type)] -> Exp Pos -> TypecheckM Type
typecheckCaseAlternative [] right = typecheck right
typecheckCaseAlternative ((EVar _ (LIdent vname), expectedType):es) right =
    local (addVariableEnv vname expectedType) $ typecheckCaseAlternative es right
typecheckCaseAlternative ((EInt _ _, TInt):es) right = typecheckCaseAlternative es right
typecheckCaseAlternative ((EBool _ _, TBool):es) right = typecheckCaseAlternative es right
typecheckCaseAlternative ((algVal@(EAlg _ _), algType@(TAlgebraic _)):es) right = do
    guardWithTypecheck algType algVal
    typecheckCaseAlternative es right
typecheckCaseAlternative ((ECall p (EAlg _ (UIdent algValName)) callExps, algExpectedType@(TAlgebraic _)):es) right = do
    env <- ask
    case lookupVariableEnv algValName env of
        Nothing -> throwError $ TypecheckError (AlgebraicNotInScopeError algValName) p
        (Just (TArrow constructorShape algActualType)) -> do
            guardType p algExpectedType algActualType
            let expectedArgsNum = length constructorShape
            let actualArgsNum = length callExps
            when (expectedArgsNum /= actualArgsNum) $
                throwError $ TypecheckError (ConstructorArgumentsError algValName expectedArgsNum actualArgsNum) p
            typecheckCaseAlternative (zip callExps constructorShape ++ es) right
        (Just t) -> throwError $ TypecheckError (UnexpectedTypeError algExpectedType t) p
typecheckCaseAlternative ((e, t):_) _ = throwError $ TypecheckError (CaseTypeMismatchError t) (pos e)

-- returns type of right of (any) alternative
typecheckCaseExpression :: [Case Pos] -> Type -> TypecheckM Type
typecheckCaseExpression [] _ = return TUnit
typecheckCaseExpression [Case _ left right] matchType = typecheckCaseAlternative [(left, matchType)] right
typecheckCaseExpression (Case p left right:cs) matchType = do
    nextAlternativesType <- typecheckCaseExpression cs matchType
    thisAlternativeType <- typecheckCaseAlternative [(left, matchType)] right
    guardType p thisAlternativeType nextAlternativesType
    return thisAlternativeType

instance Typecheckable (Exp Pos) where
    typecheck (EInt _ _) = return TInt
    typecheck (EBool _ _) = return TBool
    typecheck (EUnit _ _) = return TUnit
    typecheck (EVar p (LIdent vname)) = do
        env <- ask
        case lookupVariableEnv vname env of
            Nothing -> throwError $ TypecheckError (VariableNotInScopeError vname) p
            Just t -> return t
    typecheck (EAlg p (UIdent aname)) = do
        env <- ask
        case lookupVariableEnv aname env of
            Nothing -> throwError $ TypecheckError (AlgebraicNotInScopeError aname) p
            Just t -> return t
    typecheck (EOr _ e1 e2) = guardBinaryType e1 e2 TBool TBool >> return TBool
    typecheck (EAnd _ e1 e2) = guardBinaryType e1 e2 TBool TBool >> return TBool
    typecheck (EEq p e1 e2) = do
        t1 <- typecheck e1
        case t1 of
            (TArrow _ _) -> throwError $ TypecheckError (NotComparable t1) p
            _ -> do
                guardWithTypecheck t1 e2
                return TBool
    typecheck (ENeq p e1 e2) = typecheck (EEq p e1 e2)
    typecheck (ELt _ e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TBool
    typecheck (EGt _ e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TBool
    typecheck (ELe _ e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TBool
    typecheck (EGe _ e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TBool
    typecheck (EAdd _ e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TInt
    typecheck (ESub _ e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TInt
    typecheck (EMult _ e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TInt
    typecheck (EDiv _ e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TInt
    typecheck (EMod _ e1 e2) = guardBinaryType e1 e2 TInt TInt >> return TInt
    typecheck (EIfte _ condition leftExp rightExp) = do
        guardWithTypecheck TBool condition
        leftType <- typecheck leftExp
        guardWithTypecheck leftType rightExp
        return leftType
    typecheck (ECase _ match alternatives) = do
        matchType <- typecheck match
        typecheckCaseExpression alternatives matchType
    typecheck (ELambda _ typedParams body) = do
        let paramNames = map getName typedParams
        let paramPTypes = map getPType typedParams
        paramTypes <- mapM typecheck paramPTypes
        bodyType <- local (addVariablesEnv paramNames paramTypes) $ typecheck body
        return $ TArrow paramTypes bodyType
    typecheck (ECall p funExp argExpressions) = do
        funType <- typecheck funExp
        case funType of
            (TArrow argExpectedTypes resultExpectedType) -> do
                let numExpectedArgs = length argExpectedTypes
                let numActualArgs = length argExpressions
                when (numExpectedArgs < numActualArgs) $
                    throwError $ TypecheckError (TooManyArgumentsError numExpectedArgs numActualArgs) p
                mapM_ (uncurry guardWithTypecheck) (zip argExpectedTypes argExpressions)
                if numExpectedArgs == numActualArgs
                    then return resultExpectedType
                    else return $ TArrow (drop numActualArgs argExpectedTypes) resultExpectedType
            _ -> throwError $ TypecheckError (NonArrowTypeError funType) p

instance Typecheckable (Body Pos) where
    typecheck (Body _ [] e) = typecheck e
    typecheck (Body p (DPut _ expr:ds) e) = do
        _ <- typecheck expr
        typecheck (Body p ds e)
    typecheck (Body p (DVal _ (TypedIdent _ (LIdent valName) expectedPType) valExp:ds) bodyExp) = do
        expectedType <- typecheck expectedPType
        guardWithTypecheck expectedType valExp
        local (addVariableEnv valName expectedType) $ typecheck (Body p ds bodyExp)
    typecheck (Body p (DFun _ (LIdent funName) typedParams bodyPType body:ds) bodyExp) = do
        bodyExpectedType <- typecheck bodyPType
        let paramNames = map getName typedParams
        let paramPTypes = map getPType typedParams
        paramTypes <- mapM typecheck paramPTypes
        let funType = TArrow paramTypes bodyExpectedType
        local (addVariableEnv funName funType . addVariablesEnv paramNames paramTypes) $
            guardWithTypecheck bodyExpectedType body
        local (addVariableEnv funName funType) $ typecheck (Body p ds bodyExp)
    typecheck (Body p (DAlg _ (UIdent typeName) values:ds) bodyExp) = do
        let algType = TAlgebraic typeName
        let valueNames = map getNameA values
        let valueParamPTypes = map getPTypes values
        valueParamTypes <- mapM (mapM typecheck) valueParamPTypes
        let valueTypes =
                map
                    (\types ->
                         if null types
                             then algType
                             else TArrow types algType)
                    valueParamTypes
        local (addVariablesEnv valueNames valueTypes) $ typecheck (Body p ds bodyExp)