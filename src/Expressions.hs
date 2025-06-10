module Expressions where

import Data.List.NonEmpty (NonEmpty)
import Data.Text.Lazy (Text)

{- A program is a list of procedures -}
newtype Program = Program {procBlocks :: [Proc]}
    deriving (Show)

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
 -}
data Type = BoolT
    deriving (Eq, Show)

{- An expression is:
 - boolean
 - binary operator
 - unary operator
-}
data Expr
    = IdE Text
    | BoolE Bool
    | Universal Text Expr
    | Existential Text Expr
    | BinOpE BinOp Expr Expr
    | UnaryOpE UnOp Expr
    deriving (Show)

data Value = BoolV Bool
    deriving (Show, Eq)

{- Binary operators -}
data BinOp
    = Equiv
    | Impl
    | And
    | Or
    deriving (Show)

{- Unary operators -}
data UnOp = Not
    deriving (Show)
