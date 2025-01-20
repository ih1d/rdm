module Main where

import Control.Monad (when)
import Data.Text.Lazy.IO (readFile)
import Parser (parser)
import Semantics
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Prelude hiding (readFile)

run :: FilePath -> IO ()
run f = do
    let ext = reverse $ take 3 $ reverse f
    when (ext /= "gcl") $ do
        putStrLn "expected file extension gcl"
        exitFailure
    contents <- readFile f
    case parser contents of
        Right program -> do
            st <- initStack
            semEv <- runSemantics st (analyzeProgram program)
            print semEv
            exitSuccess
        Left err -> print err >> exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> run f
        _ -> putStrLn "Usage: gcl FILE"
