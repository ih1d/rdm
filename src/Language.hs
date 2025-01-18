module Language (
    gclLexer,
) where

import Control.Monad.Identity (Identity)
import Data.Text.Lazy (Text)
import Text.Parsec (alphaNum, char, letter, oneOf, (<|>))
import Text.Parsec.Token (
    GenLanguageDef (..),
    GenTokenParser (..),
    makeTokenParser,
 )

gclDef :: GenLanguageDef Text st Identity
gclDef =
    LanguageDef
        { commentStart = "/*"
        , commentEnd = "*/"
        , commentLine = "//"
        , nestedComments = True
        , identStart = letter
        , identLetter = alphaNum <|> char '_'
        , opStart = opLetter gclDef
        , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , reservedOpNames = gclOps
        , reservedNames = gclKeywords
        , caseSensitive = True
        }

gclOps :: [String]
gclOps =
    [ "="
    , "~="
    , ":"
    , "|"
    , "||"
    , ":="
    , "->"
    , "==>"
    , "->"
    , "~"
    , "/\\"
    , "\\/"
    , "^"
    , ">="
    , ">"
    , "<="
    , "<"
    , "+"
    , "-"
    , "*"
    , "/"
    , "%"
    , "**"
    , "#"
    ]

gclKeywords :: [String]
gclKeywords =
    [ "proc"
    , "begin"
    , "end"
    , "do"
    , "od"
    , "if"
    , "fi"
    , "array"
    , "of"
    , "true"
    , "false"
    , "var"
    , "bool"
    , "i64"
    , "i32"
    , "u64"
    , "u32"
    , "chr"
    , "str"
    ]

gclLexer :: GenTokenParser Text st Identity
gclLexer = makeTokenParser gclDef
