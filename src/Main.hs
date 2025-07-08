module Main where

import Data.Text.Lazy.IO (readFile)
import Parser
import System.Environment (getArgs)
import Prelude hiding (readFile)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: rdm FILE"
        (f : _) -> readFile f >>= print . parser
