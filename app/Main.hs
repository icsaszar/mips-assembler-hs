module Main where

import Compiler
import qualified System.Environment as SysEnv
import Data.List as List
import System.Console.ParseArgs
import System.IO(hGetContents)

data ProgramArgs = Input 
                 | Sep 
                 | Help 
                 deriving (Show, Ord, Eq)

main :: IO ()
main = do
        parsedArgs <- parseArgsIO ArgsComplete args
        if gotArg parsedArgs Help then
            putStr (argsUsage parsedArgs)
        else
            runCompiler parsedArgs
    where
        args = 
            [
                Arg { 
                    argIndex = Sep,
                    argName = Just "sep",
                    argAbbr = Just 's',
                    argData = Nothing,
                    argDesc = "Use separators (\"_\") in output"},
                Arg {
                    argIndex = Help,
                    argName = Just "help",
                    argAbbr = Just 'h',
                    argData = Nothing,
                    argDesc = "Show this help"},
                Arg {
                    argIndex = Input,
                    argName = Nothing,
                    argAbbr = Nothing,
                    argData = (argDataOptional "input" ArgtypeString),
                    argDesc = "Input source. If ommitted stdin will be used"}
                
            ]

runCompiler :: Args ProgramArgs -> IO ()
runCompiler parsedArgs = do
    hInput <- getArgStdio parsedArgs Input ReadMode
    input <- hGetContents hInput
    compile sep input >>= putStrLn where
        sep = if (gotArg parsedArgs Sep) then "_" else ""