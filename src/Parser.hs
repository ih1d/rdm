module Parser (parser) where

import Control.Monad (void)
import Data.Text.Lazy (Text, pack)
import Expressions
import Language
import Text.Parsec (
    ParseError,
    choice,
    many1,
    oneOf,
    optionMaybe,
    parse,
    try,
    (<|>),
 )
import Text.Parsec.Text.Lazy (Parser)
import Text.Parsec.Token (
    GenTokenParser (..),
 )

-- parser helpers --
rslIdentifier :: Parser Text
rslIdentifier = pack <$> rslLexeme (identifier rslLexer)

rslInt :: Parser Int
rslInt = fromInteger <$> integer rslLexer

rslParens :: Parser a -> Parser a
rslParens = parens rslLexer

rslReserved :: String -> Parser ()
rslReserved = reserved rslLexer

rslSemiSep :: Parser a -> Parser [a]
rslSemiSep = semiSep rslLexer

rslCommaSep :: Parser a -> Parser [a]
rslCommaSep = commaSep rslLexer

rslReservedOp :: String -> Parser ()
rslReservedOp = reservedOp rslLexer

rslLexeme :: Parser p -> Parser p
rslLexeme p = p <* whitespace

whitespace :: Parser ()
whitespace =
    choice
        [ simpleWhitespace *> whitespace
        , pure ()
        ]
  where
    simpleWhitespace = void $ many1 (oneOf " \t\n")

-- parser helpers --

{- parse a binary operator
rslBinaryOp :: String -> (a -> a -> a) -> Assoc -> Operator Text () Identity a
rslBinaryOp txt f = Infix (rslReservedOp txt >> pure f)

 parse a unary operator
rslUnaryOp :: String -> (a -> a) -> Operator Text () Identity a
rslUnaryOp txt f = Prefix (rslReservedOp txt >> pure f)

table :: OperatorTable Text () Identity Expr
table =
    [
        [ rslUnaryOp "~" (UnaryOpE Not)
        ]
    ,
        [ rslBinaryOp "==" (BinOpE Equiv) AssocNone
        ]
    , [rslBinaryOp "/\\" (BinOpE And) AssocRight]
    , [rslBinaryOp "\\/" (BinOpE Or) AssocRight]
    , [rslBinaryOp "==>" (BinOpE Impl) AssocRight]
    ]
-}

-- parse type declaration
rslTypeDef :: Parser Declaration
rslTypeDef = do
    rslReserved "type"
    TypeDecl <$> rslCommaSep (try rslTypeDefUnion <|> rslTypeDefSort)

rslTypeDefUnion :: Parser TypeDef
rslTypeDefUnion = do
    t <- rslIdentifier
    rslReservedOp "="
    Union t <$> rslIdentifier

rslTypeDefSort :: Parser TypeDef
rslTypeDefSort = Sort <$> rslIdentifier

-- parse value declaration
rslValueDef :: Parser ValueDef
rslValueDef = do
    rslReserved "value"
    v <- rslIdentifier
    rslReservedOp ":"
    ValueDef v <$> (rslIdentifier <|> rslTypeExpr)
  where
    rslTypeExpr = do
        t1 <- rslIdentifier
        rslReservedOp "><"
        t2 <- rslIdentifier
        rslReservedOp "->"
        t3 <- rslIdentifier <|> ("Nat" <$ rslReserved "Nat") <|> ("Bool" <$ rslReserved "Bool")
        pure $ t1 <> t2 <> t3

-- parse declarations
rslDeclaration :: Parser Declaration
rslDeclaration = rslTypeDef -- <|> rslValueDef <|> rslAxiomDef

-- parse extension
rslExtend :: Parser Text
rslExtend = do
    rslReserved "extend"
    e <- rslIdentifier
    rslReserved "with"
    pure e

-- parse a class
rslClass :: Parser Class
rslClass = do
    cname <- rslIdentifier
    rslReservedOp "="
    extension <- optionMaybe rslExtend
    rslReserved "class"
    Class cname extension <$> many1 rslDeclaration

parser :: Text -> Either ParseError Class
parser = parse rslClass "rsl"
