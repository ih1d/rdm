module Expressions where

import GHC.Arr (Array)
import Data.Text.Lazy (Text)
import Data.Word (Word32)
import Data.List.NonEmpty (NonEmpty)
import Data.Int (Int32)

{- Procedure
 - consists of name, 
 - a list of parameters,
 - maybe local variables, and
 - body
-}
data Proc = Proc 
    { procName :: Text
    , parameters :: [(Text, Type)]
    , localVariables :: [(Text, Type)]
    , body :: NonEmpty Expr
    }
    deriving (Show)

{- Types
 - booleans
 - 64 bit signed integer
 - 32 bit signed integer
 - 64 bit unsigned integer
 -}
data Type
    = ArrayT
    | BoolT
    | I64T
    | I32T
    | U64T
    | U32T
    | F64T
    | F32T
    deriving (Show)

{- An expression is:
 - boolean 
 - number
 - array
 - identifier
 - if statement
 - binary operator
 - unary operator
 - integer
 - unsigned integer
-}
data Expr
    = ArrayE (Array Int Value) 
    | BoolE Bool
    | I64E Int
    | I32E Int32
    | U64E Word
    | U32E Word32
    | F64E Double
    | F32E Float
    | IdE Text
    | IfE Expr (NonEmpty Expr)
    | DoE Expr [Expr]
    | BinOpE BinOp Expr Expr
    | UnaryOpE UnOp Expr
    deriving (Show)

data Value
    = ArrayV (Array Int Value)
    | BoolV Bool 
    | I64V Int
    | I32V Int32
    | U64V Word
    | U32V Word32
    | F64V Double
    | F32V Float
    deriving (Show)
 
{- Binary operators -}
data BinOp
    = Equal
    | NotEqual
    | Impl
    | And
    | Or
    | Gt
    | GtEq
    | Lt
    | LtEq
    | Add
    | Sub
    | Mul
    | Div
    | Exp
    | Assign
    deriving (Show)

{- Unary operators -}
data UnOp = Not
    deriving (Show)
