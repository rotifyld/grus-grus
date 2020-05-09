module Interpreter
    ( interpret
    ) where

import Control.Monad.Except (Except)
import Data.Either (partitionEithers)
import System.IO (hPrint, hPutStrLn, stderr)

import AbsGrusGrus (Body)
import ErrM
import Executor
import IErr
import LexGrusGrus
import ParGrusGrus
import Typechecker
import Utils (Pos)

interpret :: IO ()
interpret = do
    putStrLn ""
    programStr <- getContents
    case runParser programStr of
        (Bad str) -> hPutStrLn stderr str
        (Ok body) -> do
            eitherValueType <- runInterpreter body
            case eitherValueType of
                (Left err) -> hPrint stderr err
                (Right (t, vals)) -> do
                    putStrLn $ ":: " ++ show t
                    mapM_ print vals

runParser :: String -> Err (Body Pos)
runParser s = pBody (myLexer s)

runInterpreter :: Body Pos -> IO (Either IError (Type, [Value]))
runInterpreter body =
    case runTypecheckM (typecheck body) of
        (Left err) -> return $ Left err
        (Right t) -> do
            e <- runExecuteM (execute body)
            case e of
                (Left err) -> return $ Left err
                (Right vals) -> return $ Right (t, vals)