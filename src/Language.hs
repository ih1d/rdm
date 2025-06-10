module Language (
    rccLexer,
) where

import Control.Monad.Identity (Identity)
import Data.Text.Lazy (Text)
import Text.Parsec (alphaNum, char, letter, oneOf, (<|>))
import Text.Parsec.Token (
    GenLanguageDef (..),
    GenTokenParser (..),
    makeTokenParser,
 )

rccDef :: GenLanguageDef Text st Identity
rccDef =
    LanguageDef
        { commentStart = "/*"
        , commentEnd = "*/"
        , commentLine = "//"
        , nestedComments = True
        , identStart = letter
        , identLetter = alphaNum <|> char '_'
        , opStart = opLetter rccDef
        , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , reservedOpNames = rccOps
        , reservedNames = rccKeywords
        , caseSensitive = True
        }

rccOps :: [String]
rccOps =
    [ "=="
    , "==>"
    , "~"
    , "/\\"
    , "\\/"
    , "!"
    , "?"
    ]

rccKeywords :: [String]
rccKeywords =
    [ "true"
    , "false"
    ]

rccLexer :: GenTokenParser Text st Identity
rccLexer = makeTokenParser rccDef
