module Main where

import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO (readFile)
import Parser
import System.Environment (getArgs)
import Prelude hiding (readFile)

run :: Text -> IO ()
run input =
    case parser input of
        Left err -> print err
        Right m -> print m
       
main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> readFile f >>= run
        _ -> putStrLn "Usage: rdm [FILE]"
