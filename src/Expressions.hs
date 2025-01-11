module Expressions where

import Data.Set (Set)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Text.Lazy (Text)
import Data.Void (Void)
import Numeric.Natural

{- Module
 - consists of name, declarations (type, values and/or axioms),
 - and of a possible extension
-}
data Module = Module
    { moduleName :: Text
    , declarations :: NonEmpty Declaration
    , extension :: Maybe Text
    }
    deriving (Show)

{- Declaration
 - TypeDecl consists of list of Type declarations
 - ValueDecl consists of list of Value declarations
 - AxiomDecl consists of list of Axiom declarations
-}
data Declaration
    = TypeDecl [TypeDeclaration]
    | ValueDecl [ValueDeclaration]
    | AxiomDecl [AxiomDeclaration]
    deriving (Show)

{- TypeDeclaration
 - It is a sort with a name
 - or an ADT with a name and a type expression
-}
data TypeDeclaration
    = Sort Text
    | AbstractType Text TypeExpr
    deriving (Show)

{- Native Types -}
data Type
    = NatT
    | BoolT
    | IntT
    | RealT
    | CharT
    | TextT
    | UnitT
    | AdtT Text
    deriving (Show, Eq, Ord)

{- TypeExpr
 - consists of a Type
 - a set type
 - a product: t1 >< t2
 - a function: t1 -> t2
 - an application: func(params)
-}
data TypeExpr
    = TypeTE Type
    | SetTE Type
    | InfSetTE Type
    | ProductTE [TypeExpr]
    | FuncTE TypeExpr TypeExpr
    | AppTE Text [ValueExpr] Text ValueExpr ValueExpr
    deriving (Eq, Ord, Show)

{- ValueDecl
 - an identifier
 - and a type expression
-}
data ValueDeclaration = ValueDeclaration
    { valueIdentifier :: Text
    , valueTypeExpr :: TypeExpr
    }
    deriving (Eq, Ord, Show)

-- typing same as a value declaration
type TypingList = NonEmpty ValueDeclaration 

{- AxiomDeclaration
 - a possible naming convention
 - and a value expression
-}
data AxiomDeclaration = AxiomDeclaration
    { axiomNaming :: Maybe Text
    , axiomValueExpr :: ValueExpr
    }
    deriving (Show)

{- ValueExpr
 - Boolean expr
 - Identifier
 - set expression
 - if statement
 - binary operator
 - unary operator
 - chaos expression
 - quantifier 
 - integer
 - natural
 - real
 - char
 - string/text
 - unit
 - product
 - application
-}
data ValueExpr
    = BoolVE Bool
    | IdVE Text
    | SetVE (Set ValueExpr)
    | If ValueExpr ValueExpr (Maybe [(ValueExpr, ValueExpr)]) ValueExpr
    | BinOpVE ValueBinOp ValueExpr ValueExpr
    | UnaryOpVE ValueUnOp ValueExpr
    | ChaosVE (Proxy Void)
    | QuantVE Quantifier TypingList ValueExpr
    | IntVE Int
    | NatVE Natural
    | RealVE Double
    | CharVE Char
    | TextVE Text
    | UnitVE ()
    | ProductVE [ValueExpr]
    | AppVE Text [ValueExpr]
    | FuncVE TypingList ValueExpr
    deriving (Eq, Ord, Show)

{- Binary operators -}
data ValueBinOp
    = Func
    | Equal
    | NotEqual
    | Is
    | IsIn
    | Union
    | And
    | Or
    | Impl
    | Gt
    | GtEq
    | Lt
    | LtEq
    | Add
    | Sub
    | Mul
    | Div
    | Rem
    | Exp
    deriving (Eq, Ord, Show)

{- Unary operators -}
data ValueUnOp
    = Not
    | Abs
    | IntC
    | RealC
    | Card
    | Len
    | Inds
    | Elems
    | Hd
    | Tl
    | Dom
    | Rng
    | Post
    | Pre
    deriving (Eq, Ord, Show)

{- quantifiers -}
data Quantifier = Forall | Exists | ExistsOne
    deriving (Eq, Ord, Show)
