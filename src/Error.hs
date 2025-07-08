module Error where

import Data.Text.Lazy (Text, unpack)
import Expressions

data EvalError
    = TypeError Declaration Declaration Text
    | UndeclaredVariable Text
    | RedeclaredVariable Text
    | UndeclaredProc Text
    | RedeclaredProc Text
    | ArgumentMismatch String Int Int
    | GeneralError String
    deriving (Show)

errorMessage :: EvalError -> String
errorMessage (TypeError expected actual msg) =
    "Type error: expected " ++ show expected ++ ", but got " ++ show actual ++ ", because " ++ unpack msg
errorMessage (UndeclaredVariable v) =
    "Semantic error: undeclared variable '" ++ unpack v ++ "'."
errorMessage (RedeclaredVariable v) =
    "Semantic error: variable '" ++ unpack v ++ "' is already declared in the current scope."
errorMessage (UndeclaredProc p) =
    "Semantic error: procedure '" ++ unpack p ++ "' is not defined."
errorMessage (RedeclaredProc p) =
    "Semantic error: procedure '" ++ unpack p ++ "' is already declared."
errorMessage (ArgumentMismatch p expected actual) =
    "Argument error in procedure '"
        ++ p
        ++ "': expected "
        ++ show expected
        ++ " arguments, but got "
        ++ show actual
        ++ "."
errorMessage (GeneralError msg) = msg
