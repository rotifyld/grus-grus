module Interpreter
    ( interpret
    ) where

import Executor

import AbsGrusGrus (Body)
import ErrM
import LexGrusGrus
import ParGrusGrus
import System.IO (hPutStrLn, stderr)

interpret :: IO ()
interpret = do
    putStrLn ""
    programStr <- getContents
    case runParser programStr of
        (Ok body) -> runExecuteM (execute body) >>= print
        (Bad str) -> hPutStrLn stderr str

runParser :: String -> Err Body
runParser s = pBody (myLexer s)