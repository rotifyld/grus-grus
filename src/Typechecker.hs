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
import Utils

data Env =
    Env
        { variableTable :: M.Map Name Type
        , algShapeTable :: M.Map Name [Type]
        }
    deriving (Show)

--        , algebraicTable :: M.Map Name Type
emptyEnv :: Env
emptyEnv = Env {variableTable = M.empty, algShapeTable = M.empty}

addVariableEnv :: Name -> Type -> Env -> Env
addVariableEnv vname vtype env@Env {variableTable = vtable} = env {variableTable = M.insert vname vtype vtable}

addVariablesEnv :: [Name] -> [Type] -> Env -> Env
addVariablesEnv vnames vtypes env = foldl (\e (n, t) -> addVariableEnv n t e) env (zip vnames vtypes)

lookupVariableEnv :: Name -> Env -> Maybe Type
lookupVariableEnv vname Env {variableTable = vtable} = M.lookup vname vtable

addAlgShapeEnv :: Name -> [Type] -> Env -> Env
addAlgShapeEnv name shape env@Env {algShapeTable = astable} = env {algShapeTable = M.insert name shape astable}

addAlgShapesEnv :: [Name] -> [[Type]] -> Env -> Env
addAlgShapesEnv names shapes env = foldl (\e (n, s) -> addAlgShapeEnv n s e) env (zip names shapes)

lookupAlgShapeEnv :: Name -> Env -> Maybe [Type]
lookupAlgShapeEnv name Env {algShapeTable = astable} = M.lookup name astable

type TypecheckM = ReaderT Env (Except IError)

runTypecheckM :: TypecheckM a -> Either IError a
runTypecheckM typecheckable = runExcept (runReaderT typecheckable emptyEnv)

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
typecheckCaseAlternative ((EVar (Ident vname), expectedType):es) right =
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
    typecheck (EVar (Ident vname)) = do
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
    typecheck (Body (DPut _:ds) e) = typecheck (Body ds e)
    typecheck (Body (DVal (TypedIdent (Ident valName) expectedPType) valExp:ds) bodyExp) = do
        expectedType <- typecheck expectedPType
        guardWithTypecheck expectedType valExp
        local (addVariableEnv valName expectedType) $ typecheck (Body ds bodyExp)
    typecheck (Body (DFun (Ident funName) typedParams bodyPType body:ds) bodyExp) = do
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