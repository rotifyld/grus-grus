module InterpreterError where

import Control.Monad.Except (Except)

data IError
    = ITCError
    | IEError ExecuteError

instance Show IError where
    show ITCError = "ITCError"
    show (IEError err) = "Runtime error: " ++ show err

data ExecuteError =
    DivByZero

instance Show ExecuteError where
    show DivByZero = "Divide by zero"