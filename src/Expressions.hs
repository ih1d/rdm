module Expressions where

import Data.Text.Lazy (Text)

-- Class
-- consists of name of a list of declarations
-- and may be an extension from another class
data Class = Class
    { className :: Text
    , extend :: Maybe Text
    , declarations :: [Declaration]
    }
    deriving (Show)

-- Declaration
-- either a type, value or axiom
data Declaration
    = TypeDecl [TypeDef]
    | ValueDecl [ValueDef]
    | AxiomDecl [AxiomDef]
    deriving (Show)

-- Type Definition
-- defines abstract datatypes
data TypeDef
    = Sort Text
    | Union Text Text
    deriving (Show)

-- Value Definition
-- defines some identifier with some type expression
data ValueDef = ValueDef
    { valueDef :: Text
    , typeExpr :: TypeExpr
    }
    deriving (Show)

type TypeExpr = Text

type ValueExpr = Text

-- Axiom Defintion
-- defines a list of value expressions, and
-- maybe an optional naming
data AxiomDef = AxiomDef
    { valueExprs :: ValueExpr
    , optAxiomName :: Text
    }
    deriving (Show)
