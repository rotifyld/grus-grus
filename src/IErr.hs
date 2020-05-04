module IErr where

import Control.Monad.Except (Except)

import AbsGrusGrus
import TypecheckerUtils (Type)
import Utils

data IError
    = TypecheckError TCError
    | ExecutionError EError

data TCError
    = UnexpectedTypeError
          { expected :: Type
          , actual :: Type
          }
    | VariableNotInScopeError Name
    | AlgebraicNotInScopeError Name
    | NonArrowTypeError Type
    | TooManyArgumentsError Int Int
    | ConstructorArgumentsError Int Int
    | EmptyCaseAlternativesListError
    | CaseTypeMismatchError Type Exp

data EError
    = DivideByZeroError
    | NoPatternMatchedError
    | UnexpectedTypeExecutionError
    | VariableNotInScopeExecutionError

instance Show IError where
    show (TypecheckError err) = "Type check error: " ++ show err
    show (ExecutionError err) = "Runtime error: " ++ show err

instance Show TCError where
    show UnexpectedTypeError {expected = etype, actual = atype} =
        unlines ["Unexpected type.", "  Expected:", "    " ++ show etype, "  Actual", "    " ++ show atype]
    show (VariableNotInScopeError vname) = "Variable \"" ++ vname ++ "\" not in scope."
    show (AlgebraicNotInScopeError aname) = "Algebraic value \"" ++ aname ++ "\" not in scope."
    show (NonArrowTypeError t) =
        unlines ["Expression is not callable.", "  Expected an arrow type.", "  Actual:", "    " ++ show t]
    show (TooManyArgumentsError expected actual) =
        "Too many arguments. Expected " ++ show expected ++ ". Actual " ++ show actual
    show (ConstructorArgumentsError expected actual) =
        "Invalid number of arguments in constructor. Expected " ++ show expected ++ ". Actual " ++ show actual
    show EmptyCaseAlternativesListError = "Empty case list of alternatives."
    show (CaseTypeMismatchError t exp) =
        unlines
            [ "Coulnd't match types in case expression."
            , "  Expected:"
            , "    " ++ show t
            , "  Actual: type arising from expression:"
            , "    " ++ show exp
            ]

instance Show EError where
    show DivideByZeroError = "Divide by zero."
    show NoPatternMatchedError = "No pattern matched."
    show UnexpectedTypeExecutionError = "Invalid type error."