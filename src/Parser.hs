{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Parser (parser) where

import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Data.Int (Int32)
import Data.Text.Lazy (Text, pack)
import Data.Word (Word32)
import Expressions
import Language
import Text.Parsec (
    ParseError,
    choice,
    many1,
    oneOf,
    parse,
    (<|>),
 )
import Text.Parsec.Expr (Assoc (..), Operator (..), OperatorTable, buildExpressionParser)
import Text.Parsec.Text.Lazy (Parser)
import Text.Parsec.Token (
    GenTokenParser (..),
    parens,
 )

-- parser helpers --
rccIdentifier :: Parser Text
rccIdentifier = pack <$> rccLexeme (identifier rccLexer)

rccInt :: Parser Int
rccInt = fromInteger <$> integer rccLexer

rccInt32 :: Parser Int32
rccInt32 = fromInteger <$> integer rccLexer

rccWord :: Parser Word
rccWord = fromInteger <$> integer rccLexer

rccWord32 :: Parser Word32
rccWord32 = fromInteger <$> integer rccLexer

rccDouble :: Parser Double
rccDouble = float rccLexer

rccFloat :: Parser Float
rccFloat = realToFrac <$> float rccLexer

rccChar :: Parser Char
rccChar = charLiteral rccLexer

rccStr :: Parser Text
rccStr = pack <$> stringLiteral rccLexer

rccParens :: Parser a -> Parser a
rccParens = parens rccLexer

rccReserved :: String -> Parser ()
rccReserved = reserved rccLexer

rccSemiSep :: Parser a -> Parser [a]
rccSemiSep = semiSep rccLexer

rccCommaSep :: Parser a -> Parser [a]
rccCommaSep = commaSep rccLexer

rccReservedOp :: String -> Parser ()
rccReservedOp = reservedOp rccLexer

rccLexeme :: Parser p -> Parser p
rccLexeme p = p <* whitespace

whitespace :: Parser ()
whitespace =
    choice
        [ simpleWhitespace *> whitespace
        , pure ()
        ]
  where
    simpleWhitespace = void $ many1 (oneOf " \t\n")

-- parser helpers --

-- parse a type
rccType :: Parser Type
rccType =
    BoolT <$ rccReserved "bool"

-- parse a boolean value
rccBoolExpr :: Parser Expr
rccBoolExpr = do
    boolVal <- rccLexeme (True <$ rccReserved "true") <|> (False <$ rccReserved "false")
    pure $ BoolE boolVal

-- parse an terminal expression
rccTermExpr :: Parser Expr
rccTermExpr = rccBoolExpr

-- parse a binary operator
rccBinaryOp :: String -> (a -> a -> a) -> Assoc -> Operator Text () Identity a
rccBinaryOp txt f = Infix (rccReservedOp txt >> pure f)

-- parse a unary operator
rccUnaryOp :: String -> (a -> a) -> Operator Text () Identity a
rccUnaryOp txt f = Prefix (rccReservedOp txt >> pure f)

table :: OperatorTable Text () Identity Expr
table =
    [
        [ rccUnaryOp "~" (UnaryOpE Not)
        , rccUnaryOp "!" (UnaryOpE Universal)
        , rccUnaryOp "?" (UnaryOpE Existential)
        ]
    ,
        [ rccBinaryOp "==" (BinOpE Equiv) AssocNone
        ]
    , [rccBinaryOp "/\\" (BinOpE And) AssocRight]
    , [rccBinaryOp "\\/" (BinOpE Or) AssocRight]
    , [rccBinaryOp "==>" (BinOpE Impl) AssocRight]
    ]

-- parse term expressions
rccTerms :: Parser Expr
rccTerms = buildExpressionParser table rccTermExpr

-- parse an expression
rccExpr :: Parser Expr
rccExpr = rccTerms

parser :: Text -> Either ParseError Expr
parser = parse rccExpr "rcc"
