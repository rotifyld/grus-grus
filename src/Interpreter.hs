module Interpreter
    ( interpret
    ) where

import Executor
import InterpreterError

import AbsGrusGrus (Body)
import Control.Monad.Except (Except)
import ErrM
import LexGrusGrus
import ParGrusGrus
import System.IO (hPutStrLn, stderr, hPrint)

interpret :: IO ()
interpret = do
    putStrLn ""
    programStr <- getContents
    case runParser programStr of
        (Bad str) -> hPutStrLn stderr str
        (Ok body) -> do
            either <- runInterpreter body
            case either of
                (Left err) -> hPrint stderr err
                (Right val) -> print val

runParser :: String -> Err Body
runParser s = pBody (myLexer s)

runInterpreter :: Body -> IO (Either IError Value)
runInterpreter body = runExecuteM (execute body)