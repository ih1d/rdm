{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Parser (parser) where

import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Data.Text.Lazy (Text, pack)
import Expressions
import Language
import Text.Parsec (
    ParseError,
    char,
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

-- parse an identifier
rccIdExpr :: Parser Expr
rccIdExpr = IdE <$> rccIdentifier

-- parse an terminal expression
rccTermExpr :: Parser Expr
rccTermExpr = rccBoolExpr <|> rccIdExpr

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

-- parse existential quantifier
rccQuantifierxpr :: Parser Expr
rccQuantifierxpr = do
    void $ rccLexeme (char '(')
    func <- (Existential <$ rccLexeme (char '?')) <|> (Universal <$ rccLexeme (char '!'))
    ident <- rccIdentifier
    void $ rccLexeme (char '.')
    expr <- rccExpr
    void $ rccLexeme (char ')')
    pure $ func ident expr

-- parse an expression
rccExpr :: Parser Expr
rccExpr = rccQuantifierxpr <|> rccTerms

parser :: Text -> Either ParseError Expr
parser = parse rccExpr "rcc"
