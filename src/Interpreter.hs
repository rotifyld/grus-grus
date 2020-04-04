module Interpreter
    ( interpret
    ) where

import Executor

import AbsGrusGrus (Body)
import ErrM
import LexGrusGrus
import ParGrusGrus

interpret :: IO ()
interpret = do
    putStrLn ""
    interact runInterpreter
    putStrLn "\n"

runInterpreter :: String -> String
runInterpreter s =
    case runParser s of
        (Ok body) -> show $ runExecuteM (execute body)
        (Bad str) -> show str

runParser :: String -> Err Body
runParser s = pBody (myLexer s)