module Main where

import Control.Monad (when)
import Data.Text.Lazy.IO (readFile)
import Parser (parser)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Prelude hiding (readFile)

run :: FilePath -> Bool -> IO ()
run f t = do
    let ext = reverse $ take 3 $ reverse f
    when (ext /= "gcl") $ do
        putStrLn "expected file extension gcl"
        putStrLn "Usage: gcl [-t|--transpile] FILE" 
        exitFailure
    contents <- readFile f
    case parser contents t of
        Right program -> print program >> exitSuccess
        Left err -> print err >> exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        [t, f] -> 
            if t == "-t" || t == "--transpile" 
                then run f True
                else do
                    putStrLn $ "Flag " ++ t ++ " unrecognizable"
                    putStrLn "Usage: gcl [-t|--transpile] FILE" 
                    exitFailure
        [f] -> run f False
        _ -> putStrLn "Usage: gcl [-t|--transpile] FILE" >> exitFailure
