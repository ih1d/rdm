module Error where

import Expressions
import Data.Text.Lazy (Text, unpack)

data EvalError
    = TypeError Value Value
    | UndeclaredVariable Text
    | RedeclaredVariable Text
    | UndeclaredProc Text
    | ArgumentMismatch String Int Int
    | GeneralError String
    deriving (Show)

errorMessage :: EvalError -> String
errorMessage (TypeError expected actual) =
    "Type error: expected " ++ show expected ++ ", but got " ++ show actual ++ "."
errorMessage (UndeclaredVariable v) =
    "Semantic error: undeclared variable '" ++ (unpack v) ++ "'."
errorMessage (RedeclaredVariable v) =
    "Semantic error: variable '" ++ (unpack v) ++ "' is already declared in the current scope."
errorMessage (UndeclaredProc p) =
    "Semantic error: procedure '" ++ (unpack p) ++ "' is not defined."
errorMessage (ArgumentMismatch p expected actual) =
    "Argument error in procedure '"
        ++ p
        ++ "': expected "
        ++ show expected
        ++ " arguments, but got "
        ++ show actual
        ++ "."
errorMessage (GeneralError msg) = msg

