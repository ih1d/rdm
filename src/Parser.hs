module Parser (parser) where

import Control.Monad (void)
import Data.Text.Lazy (Text, pack)
import Expressions
import Language
import Text.Parsec (
    ParseError,
    between,
    char,
    choice,
    many,
    many1,
    oneOf,
    optionMaybe,
    parse,
    string,
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

-- parse if expr
parseIfExpr :: Parser ValueExpr
parseIfExpr = do
    rslReserved "if"
    cond <- rslLexeme $ pack <$> string "x < 0"
    rslReserved "then"
    thn <- rslLexeme $ pack <$> string "x + 1"
    conds <- many parseConds
    rslReserved "else"
    els <- rslLexeme $ pack <$> string "0"
    rslReserved "end"
    pure $ IfE cond thn conds  els
  where
    parseConds :: Parser (Text, Text)
    parseConds = do
        rslReserved "elsif"
        cond <- rslLexeme $ pack <$> string "x > 0"
        rslReserved "then"
        thn <- rslLexeme $ pack <$> string "x + 1"
        pure (cond, thn)

-- parse type declaration
rslTypeDecl :: Parser Declaration
rslTypeDecl = do
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
rslValueDecl :: Parser Declaration
rslValueDecl = do
    rslReserved "value"
    ValueDecl <$> rslCommaSep (try complexValueDef <|> simpleValueDef)
  where
    simpleValueDef = do
        v <- rslIdentifier
        rslReservedOp ":"
        ValueDef v <$> rslIdentifier
    complexValueDef = do
        v <- rslIdentifier
        rslReservedOp ":"
        t1 <- rslIdentifier
        rslReservedOp "><"
        t2 <- rslIdentifier
        rslReservedOp "->"
        t3 <- rslIdentifier <|> ("Nat" <$ rslReserved "Nat") <|> ("Bool" <$ rslReserved "Bool")
        pure $ ValueDef v (t1 <> t2 <> t3)

-- parse axiom declarations
rslAxiomDecl :: Parser Declaration
rslAxiomDecl = do
    rslReserved "axiom"
    AxiomDecl <$> rslCommaSep simpleValueExpr
  where
    axiomHeading = between (char '[') (char ']') rslIdentifier
    simpleValueExpr = do
        h <- optionMaybe axiomHeading
        ident <- rslIdentifier
        rslReserved "is"
        i <- parseIfExpr
        pure $ AxiomDef ident i h

-- parse declarations
rslDeclaration :: Parser Declaration
rslDeclaration = rslTypeDecl <|> rslValueDecl <|> rslAxiomDecl

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
    decls <- many1 rslDeclaration
    rslReserved "end"
    pure $ Class cname extension decls

parser :: Text -> Either ParseError Class
parser = parse rslClass "rsl"
