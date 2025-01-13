module Main where

import Control.Monad (when)
import Parser (parser)
import System.Environment (getArgs)
import Data.Text.Lazy.IO (readFile)
import System.Exit (exitSuccess, exitFailure)
import Prelude hiding (readFile)

run :: FilePath -> IO ()
run f = do
    let ext = reverse $ take 3 $ reverse f
    when (ext /= "gcl") $ do
        putStrLn "expected file extension gcl" 
        exitFailure
    contents <- readFile f
    let res = parser contents
    print res
    exitSuccess

main :: IO ()
main = do
    args <- getArgs
    case args of
        [f] -> run f
        _ -> putStrLn "Usage: gcl FILE"
