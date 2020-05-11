module Main where

import Interpreter (interpret)
import System.Environment (getArgs)

invalidUseStr :: String
invalidUseStr =
    unlines
        [ "Invalid usage."
        , "   Usage: ./interpreter PROGRAM           % will read from PROGRAM file"
        , "      or: ./interpreter                   % will read from standard input"
        ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            s <- getContents
            interpret s False
        [filename] -> do
            s <- readFile filename
            interpret s False
        _ -> putStr invalidUseStr