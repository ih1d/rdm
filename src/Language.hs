module Language (
    rslLexer,
) where

import Control.Monad.Identity (Identity)
import Data.Text.Lazy (Text)
import Text.Parsec (alphaNum, char, letter, oneOf, (<|>))
import Text.Parsec.Token (
    GenLanguageDef (..),
    GenTokenParser (..),
    makeTokenParser,
 )

rslDef :: GenLanguageDef Text st Identity
rslDef =
    LanguageDef
        { commentStart = "/*"
        , commentEnd = "*/"
        , commentLine = "//"
        , nestedComments = True
        , identStart = letter
        , identLetter = alphaNum <|> char '_' <|> char '-'
        , opStart = opLetter rslDef
        , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , reservedOpNames = rslOps
        , reservedNames = rslKeywords
        , caseSensitive = True
        }

rslOps :: [String]
rslOps =
    [ "="
    , ":"
    , "><"
    , ":-"
    , "->"
    ]

rslKeywords :: [String]
rslKeywords =
    [ "class"
    , "type"
    , "value"
    , "axiom"
    , "is"
    , "forall"
    , "isin"
    , "union"
    , "end"
    , "extend"
    , "with"
    , "card"
    , "Nat"
    , "Bool"
    , "Int"
    , "Real"
    , "Char"
    , "Text"
    , "Unit"
    , "true"
    , "false"
    , "if"
    , "then"
    , "else"
    , "elsif"
    ]

rslLexer :: GenTokenParser Text st Identity
rslLexer = makeTokenParser rslDef
