module IErr where

import Control.Monad.Except (Except)

import Utils

data IError
    = TypecheckError TCError
    | ExecutionError EError

data TCError
    = UnexpectedTypeError
          { expected :: Type
          , actual :: Type
          }
    | VariableNotInScopeError String
    | NonArrowTypeError Type
    | TooManyArgumentsError Int Int

data EError
    = DivideByZeroError
    | NoPatternMatcherErrorTMP

instance Show IError where
    show (TypecheckError err) = "Type check error: " ++ show err
    show (ExecutionError err) = "Runtime error: " ++ show err

instance Show TCError where
    show UnexpectedTypeError {expected = etype, actual = atype} =
        unlines ["Unexpected type.", "  Expected:", "    " ++ show etype, "  Actual", "    " ++ show atype]
    show (VariableNotInScopeError vname) = "Variable \"" ++ vname ++ "\" not in scope."
    show (NonArrowTypeError t) =
        unlines ["Expression is not callable.", "  Expected an arrow type.", "  Actual:", "    " ++ show t]
    show (TooManyArgumentsError expected actual) =
        "Too many arguments. Expected " ++ show expected ++ ". Actual " ++ show actual

instance Show EError where
    show DivideByZeroError = "Divide by zero."
    show NoPatternMatcherErrorTMP = "TMP no pattern matched"