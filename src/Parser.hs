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
    between,
    chainl1,
    char,
    choice,
    many1,
    oneOf,
    parse,
    try,
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
    func <- (ExistentialE <$ rccLexeme (char '?')) <|> (UniversalE <$ rccLexeme (char '!'))
    ident <- rccIdentifier
    void $ rccLexeme (char '.')
    expr <- rccExpr
    void $ rccLexeme (char ')')
    pure $ func ident expr

-- parse an expression
rccExpr :: Parser Expr
rccExpr = rccQuantifierxpr <|> rccTerms

-- parse an assigment
rccAssignment :: Parser Stmt
rccAssignment = do
    ident <- rccIdentifier
    rccReservedOp ":="
    Assignment ident <$> rccExpr

-- parse skip statment
rccSkipStmt :: Parser Stmt
rccSkipStmt = Skip <$ rccReserved "skip"

-- parse an if stmt
rccIfStmt :: Parser Stmt
rccIfStmt = do
    rccReserved "if"
    g <- rccExpr
    rccReserved "then"
    s <- rccStmts
    rccReserved "else"
    t <- rccStmts
    rccReserved "fi"
    pure $ If g s t

-- parse an assertion
rccAssert :: Parser Stmt
rccAssert = Assert <$> between (rccLexeme $ char '{') (rccLexeme $ char '}') rccExpr

-- parse a generalised assigment
rccGeneralAssignment :: Parser Stmt
rccGeneralAssignment = do
    assignment <- rccAssignment
    rccReservedOp "|"
    GeneralAssignment assignment <$> rccExpr

-- parse an iteration
rccDoStmt :: Parser Stmt
rccDoStmt = do
    rccReserved "do"
    g <- rccExpr
    rccReservedOp "->"
    stmts <- many1 rccStmt
    rccReserved "od"
    pure $ Do g stmts

rccSpecStmt :: Parser Stmt
rccSpecStmt = Specification <$> rccAssert <*> rccGeneralAssignment

-- parse a stmt
rccStmt :: Parser Stmt
rccStmt =
    try rccSpecStmt
        <|> try rccGeneralAssignment
        <|> rccDoStmt
        <|> rccIfStmt
        <|> rccAssert
        <|> rccSkipStmt
        <|> rccAssignment

-- parse multiple stmts
rccStmts :: Parser Stmt
rccStmts = rccStmt `chainl1` (rccLexeme (char ';') >> pure Sequential)

parser :: Text -> Either ParseError Stmt
parser = parse rccStmts "rcc"
