module Expressions where

import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.Text.Lazy (Text)
import Data.Word (Word32)
import GHC.Arr (Array)

{- A program is a list of procedures, and of assembly blocks -}
data Program = Program
    { procBlocks :: [Proc]
    , asmBlocks :: [AsmBlock]
    }
    deriving (Show)

-- an assembly block is:
data AsmBlock = AsmBlock
    { blockName :: Text
    , asm :: [AsmInstr]
    }
    deriving (Show)

-- an assembly instruction is:
type AsmInstr = ((Maybe Text), Text, Text)

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
 - 64 bit floating point number
 - 32 bit floating point number
 - character
 - string
 -}
data Type
    = ArrayT Type
    | BoolT
    | I64T
    | I32T
    | U64T
    | U32T
    | F64T
    | F32T
    | CharT
    | StrT
    deriving (Eq, Show)

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
    = ArrayMemE Text Expr
    | ArrayE Text [Expr]
    | BoolE Bool
    | I64E Int
    | I32E Int32
    | U64E Word
    | U32E Word32
    | F64E Double
    | F32E Float
    | CharE Char
    | StrE Text
    | IdE Text
    | IfE Expr [Expr] [(Expr, [Expr])] [Expr]
    | DoE Expr [Expr]
    | AppE Text [Expr]
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
    | CharV Char
    | StrV Text
    | None
    deriving (Show, Eq)

{- Binary operators -}
data BinOp
    = Assign
    | Equal
    | NotEqual
    | Impl
    | And
    | Or
    | XOr
    | Gt
    | GtEq
    | Lt
    | LtEq
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Exp
    deriving (Show)

{- Unary operators -}
data UnOp
    = Not
    | ArrLen
    deriving (Show)
