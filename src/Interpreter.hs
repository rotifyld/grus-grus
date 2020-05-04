module Interpreter
    ( interpret
    ) where

import Executor
import IErr
import Typechecker

import AbsGrusGrus (Body)
import Control.Monad.Except (Except)
import Data.Either (partitionEithers)
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
            eitherValueType <- runInterpreter body
            case eitherValueType of
                (Left errs) -> mapM_ (hPrint stderr) errs
                (Right (vals, t)) -> do
                    mapM_ print vals
                    putStrLn $ ":: " ++ show t

runParser :: String -> Err Body
runParser s = pBody (myLexer s)

runInterpreter :: Body -> IO (Either [IError] ([Value], Type))
runInterpreter body =
    case runTypecheckM (typecheck body) of
        (Left err) -> return $ Left [err]
        (Right t) -> do
            eithers <- runAllBranches body
            let (errors, values) = partitionEithers eithers
            if not (null errors)
                then return $ Left errors
                else return $ Right (values, t)

runSingleBranch :: [Branch] -> IO [Either IError Value]
runSingleBranch [] = return []
runSingleBranch (Branch exp env:bs) = do
    (either, branches) <- runExecuteM' env bs (execute exp)
    eithers <- runSingleBranch branches
    return $ (either : eithers)

runAllBranches :: Body -> IO [Either IError Value]
runAllBranches body = do
    (either, branches) <- runExecuteM (execute body)
    eithers <- runSingleBranch branches
    return $ (either : eithers)