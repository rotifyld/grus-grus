module IErr where

import TypecheckerUtils (Type)
import Utils (Name, Pos, showPos)

data IError
    = TypecheckError TypecheckError Pos
    | ExecutionError ExecutionError Pos

data TypecheckError
    = UnexpectedTypeError Type Type
    | VariableNotInScopeError Name
    | AlgebraicNotInScopeError Name
    | NonArrowTypeError Type
    | NotComparable Type
    | TooManyArgumentsError Int Int
    | ConstructorArgumentsError Name Int Int
    | CaseTypeMismatchError Type

data ExecutionError
    = DivideByZeroError
    | NoPatternMatchedError
    | UnexpectedTypeExecutionError
    | VariableNotInScopeExecutionError Name

instance Show IError where
    show (TypecheckError err pos) = unlines ["Type check error:", "At " ++ showPos pos, show err]
    show (ExecutionError err pos) = unlines ["Runtime error:", "At " ++ showPos pos, show err]

instance Show TypecheckError where
    show (UnexpectedTypeError expected actual) =
        unlines ["Unexpected type.", "  Expected:", "    " ++ show expected, "  Actual", "    " ++ show actual]
    show (VariableNotInScopeError vname) = "Variable \"" ++ vname ++ "\" not in scope."
    show (AlgebraicNotInScopeError aname) = "Algebraic value \"" ++ aname ++ "\" not in scope."
    show (NonArrowTypeError t) =
        unlines ["Expression is not callable.", "  Expected an arrow type.", "  Actual:", "    " ++ show t]
    show (NotComparable t) =
        unlines ["Expression is not comparable.", "  Expected non-arrow type.", "  Actual:", "    " ++ show t]
    show (TooManyArgumentsError expected actual) =
        "Too many arguments. Expected " ++ show expected ++ ". Actual " ++ show actual
    show (ConstructorArgumentsError name expected actual) =
        unlines
            [ "Invalid number of arguments in \"" ++ name ++ "\" constructor"
            , "  Expected: " ++ show expected
            , "  Actual: " ++ show actual
            ]
    show (CaseTypeMismatchError t) =
        unlines ["Couldn't match types in left side of case expression.", "  Expected:", "    " ++ show t]

instance Show ExecutionError where
    show DivideByZeroError = "Divide by zero"
    show NoPatternMatchedError = "No pattern matched"
    show UnexpectedTypeExecutionError = "Invalid type error"
    show (VariableNotInScopeExecutionError name) = "Variable \"" ++ name ++ "\" not in scope"