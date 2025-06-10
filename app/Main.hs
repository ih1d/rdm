module Main where

import Data.Text.Lazy.IO (getLine)
import Parser (parser)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Prelude hiding (getLine)

repl :: IO ()
repl = do
    putStr "rcc> "
    l <- getLine
    case parser l of
        Left _ -> undefined
        Right ast -> print ast >> repl

main :: IO ()
main = hSetBuffering stdout NoBuffering >> repl
