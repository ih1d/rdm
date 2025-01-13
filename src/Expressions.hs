module Expressions where

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
    = BoolT
    | I64T
    | I32T
    | U64T
    | U32T
    deriving (Show, Eq, Ord)

{- An expression is:
 - boolean 
 - identifier
 - if statement
 - binary operator
 - unary operator
 - integer
 - unsigned integer
-}
data Expr
    = BoolE Bool
    | I64E Int
    | I32E Int32
    | U64E Word
    | U32E Word32
    | IdE Text
    | IfE Expr (NonEmpty Expr)
    | Do Expr [Expr]
    | BinOpE BinOp Expr Expr
    | UnaryOpE UnOp Expr
    deriving (Eq, Ord, Show)

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
    deriving (Eq, Ord, Show)

{- Unary operators -}
data UnOp = Not
    deriving (Eq, Ord, Show)
