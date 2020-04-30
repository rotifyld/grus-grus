module Interpreter
    ( interpret
    ) where

import Executor
import IErr
import Typechecker

import AbsGrusGrus (Body)
import Control.Monad.Except (Except)
import ErrM
import LexGrusGrus
import ParGrusGrus
import System.IO (hPrint, hPutStrLn, stderr)

interpret :: IO ()
interpret = do
    putStrLn ""
    programStr <- getContents
    case runParser programStr of
        (Bad str) -> hPutStrLn stderr str
        (Ok body) -> do
            eitherValue <- runInterpreter body
            case eitherValue of
                (Left err) -> hPrint stderr err
                (Right val) -> print val

runParser :: String -> Err Body
runParser s = pBody (myLexer s)

runInterpreter :: Body -> IO (Either IError Value)
runInterpreter body =
    case runTypecheckM (typecheck body) of
        (Left err) -> return $ Left err
        (Right t) -> do
            putStrLn $ ":: " ++ show t
            runExecuteM (execute body)