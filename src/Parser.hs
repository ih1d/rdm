module Parser (parser) where

import Expressions
import Language
import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Data.Word (Word32)
import Data.List.NonEmpty (nonEmpty)
import Data.Int (Int32)
import Data.Text.Lazy (Text, pack)
import Text.Parsec (
    ParseError,
    char,
    choice,
    many,
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
gclIdentifier :: Parser Text
gclIdentifier = pack <$> gclLexeme (identifier gclLexer)

gclInt :: Parser Int
gclInt = fromInteger <$> integer gclLexer

gclInt32 :: Parser Int32
gclInt32 = fromInteger <$> integer gclLexer

gclWord :: Parser Word
gclWord = fromInteger <$> integer gclLexer

gclWord32 :: Parser Word32
gclWord32 = fromInteger <$> integer gclLexer

gclDouble :: Parser Double
gclDouble = float gclLexer

gclFloat :: Parser Float
gclFloat = realToFrac <$> float gclLexer

gclParens :: Parser a -> Parser a
gclParens = parens gclLexer

gclReserved :: String -> Parser ()
gclReserved = reserved gclLexer

gclSemiSep :: Parser a -> Parser [a]
gclSemiSep = semiSep gclLexer

gclCommaSep :: Parser a -> Parser [a]
gclCommaSep = commaSep gclLexer

gclReservedOp :: String -> Parser ()
gclReservedOp = reservedOp gclLexer

gclLexeme :: Parser p -> Parser p
gclLexeme p = p <* whitespace

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
gclType :: Parser Type
gclType =
    (BoolT <$ gclReserved "bool")
        <|> (I64T <$ gclReserved "i64")
        <|> (I32T <$ gclReserved "i32")
        <|> (U64T <$ gclReserved "u64")
        <|> (U32T <$ gclReserved "u32")
        <|> (F64T <$ gclReserved "f64")
        <|> (F32T <$ gclReserved "f32")
        <|> arrayType
    where
        arrayType = do
            gclReserved "array"
            gclReserved "of"
            t <- gclType
            pure $ ArrayT t
            

-- parse a boolean value
gclBoolExpr :: Parser Expr
gclBoolExpr = do
    boolVal <- gclLexeme (True <$ gclReserved "true") <|> (False <$ gclReserved "false")
    pure $ BoolE boolVal

-- parse an i64 value
gclI64Expr :: Parser Expr
gclI64Expr = I64E <$> gclLexeme gclInt

-- parse an i32 value
gclI32Expr :: Parser Expr
gclI32Expr = I32E <$> gclLexeme gclInt32

-- parse an u64 value
gclU64Expr :: Parser Expr
gclU64Expr = U64E <$> gclLexeme gclWord

-- parse an u32 value
gclU32Expr :: Parser Expr
gclU32Expr = U32E <$> gclLexeme gclWord32

-- parse an f64 value
gclF64Expr :: Parser Expr
gclF64Expr = F64E <$> gclLexeme gclDouble

-- parse an f32 value
gclF32Expr :: Parser Expr
gclF32Expr = F32E <$> gclLexeme gclFloat

-- parse accessing array member
gclArrayMem :: Parser Expr
gclArrayMem = do
    arr <- gclIdentifier
    void $ gclLexeme (char '[')
    idx <- gclTerms
    void $ gclLexeme (char ']')
    pure $ ArrayMemE arr idx

-- parse an terminal expression
gclTermExpr :: Parser Expr
gclTermExpr = 
    try gclArrayMem
    <|> gclIdExpr
    <|> gclBoolExpr 
    <|> gclI64Expr 
    <|> gclI32Expr 
    <|> gclU64Expr 
    <|> gclU32Expr 
    <|> gclF64Expr 
    <|> gclF32Expr 

-- parse an identifier
gclIdExpr :: Parser Expr
gclIdExpr = IdE <$> gclLexeme gclIdentifier

-- parse a binary operator
gclBinaryOp :: String -> (a -> a -> a) -> Assoc -> Operator Text () Identity a
gclBinaryOp txt f = Infix (gclReservedOp txt >> pure f)

-- parse a unary operator
gclUnaryOp :: String -> (a -> a) -> Operator Text () Identity a
gclUnaryOp txt f = Prefix (gclReservedOp txt >> pure f)

table :: OperatorTable Text () Identity Expr
table =
    [
        [ gclUnaryOp "~" (UnaryOpE Not)
        , gclUnaryOp "#" (UnaryOpE ArrLen)
        ]
    ,
        [ gclBinaryOp "**" (BinOpE Exp) AssocNone
        ]
    ,
        [ gclBinaryOp "*" (BinOpE Mul) AssocLeft
        , gclBinaryOp "/" (BinOpE Div) AssocLeft
        ]
    ,
        [ gclBinaryOp "+" (BinOpE Add) AssocLeft
        , gclBinaryOp "-" (BinOpE Sub) AssocLeft
        ]
    ,
        [ gclBinaryOp "=" (BinOpE Equal) AssocNone
        , gclBinaryOp "~=" (BinOpE NotEqual) AssocNone
        , gclBinaryOp ">" (BinOpE Gt) AssocNone
        , gclBinaryOp "<" (BinOpE Lt) AssocNone
        , gclBinaryOp ">=" (BinOpE GtEq) AssocNone
        , gclBinaryOp "<=" (BinOpE LtEq) AssocNone
        ]
    , [gclBinaryOp "/\\" (BinOpE And) AssocRight]
    , [gclBinaryOp "\\/" (BinOpE Or) AssocRight]
    , [gclBinaryOp "==>" (BinOpE Impl) AssocRight]
    , [gclBinaryOp ":=" (BinOpE Assign) AssocRight]
    ]

-- parse term expressions
gclTerms :: Parser Expr
gclTerms = buildExpressionParser table gclTermExpr 

-- parse if expression
gclIfExpr :: Parser Expr
gclIfExpr = do
    gclReserved "if"
    cnd <- gclTerms
    gclReservedOp "->"
    void $ gclLexeme (char '|')
    mthns <- many1 gclExpr 
    thns <- 
        case nonEmpty mthns of
            Nothing -> error "expecting one or more expressions after if"
            Just thns -> pure thns
    void $ gclLexeme (char '|')
    melses <- many1 gclExpr
    elses <- 
        case nonEmpty melses of
            Nothing -> error "expecting one or more expressions after if"
            Just elses -> pure elses
    gclReserved "fi"
    pure $ IfE cnd thns elses

-- parse multiway if expression
gclMultiIfExpr :: Parser Expr
gclMultiIfExpr = undefined

-- parse do expression
gclDoExpr :: Parser Expr
gclDoExpr = do
    gclReserved "do"
    cnd <- gclTerms
    gclReserved "->"
    exprs <- many gclExpr
    gclReserved "od"
    pure $ DoE cnd exprs

-- parse an expression
gclExpr :: Parser Expr
gclExpr = gclIfExpr <|> gclDoExpr <|> gclTerms 

-- parse local variables
parseLocalVariables :: Parser [(Text, Type)]
parseLocalVariables = do    
    gclReserved "var"
    vars <- gclCommaSep gclIdentifier
    gclReservedOp ":"
    t <- gclType
    pure $ map (\x -> (x,t)) vars

-- parse a varaible typing
gclVariableTyping :: Parser [(Text, Type)]
gclVariableTyping = do
    vars <- gclCommaSep gclIdentifier
    gclReservedOp ":"
    t <- gclType
    pure $ map (\x -> (x,t)) vars

-- parse formal parameters
parseFormalParams :: Parser [(Text, Type)]
parseFormalParams = do
    void $ gclLexeme (char '(')
    typings <- concat <$> gclCommaSep gclVariableTyping
    void $ gclLexeme (char ')')
    pure typings

-- parse a procedure
parseProc :: Parser Proc
parseProc = do
    gclReserved "proc"
    pname <- gclIdentifier
    fparams <- parseFormalParams   
    mlocal <- concat <$> many parseLocalVariables
    gclReserved "begin"
    mexprs <- many1 gclExpr
    exprs <- 
        case nonEmpty mexprs of
            Nothing -> error "Failed parsing body"
            Just exprs' -> pure exprs'
    gclReserved "end"
    void $ char '.'
    pure $ Proc pname fparams mlocal exprs

parser :: Text -> Either ParseError Proc
parser = parse parseProc "gcl"

