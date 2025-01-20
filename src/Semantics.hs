{-# LANGUAGE OverloadedStrings #-}

module Semantics (
    analyzeProgram,
    initStack,
    runSemantics,
) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.List.NonEmpty qualified as NE
import Error
import Expressions
import SemanticsMonad

analyzeProgram :: Program -> SemanticsM ()
analyzeProgram = analyzeProgramIter
  where
    analyzeProgramIter [] = pure ()
    analyzeProgramIter (p : ps) = do
        n <- getScope
        analyzeProc p n
        updateScope
        analyzeProgramIter ps

analyzeProc :: Proc -> Int -> SemanticsM ()
analyzeProc p@(Proc pname params locals exprs) lvl = do
    updateStack pname p
    mapM_ (\(v, t) -> updateEnv (ScopeInfo lvl (v, t))) params
    mapM_ (\(v, t) -> updateEnv (ScopeInfo lvl (v, t))) locals
    mapM_ analyzeExpr exprs

analyzeExpr :: Expr -> SemanticsM ()
analyzeExpr (I64E _) = pure ()
analyzeExpr (I32E _) = pure ()
analyzeExpr (F64E _) = pure ()
analyzeExpr (F32E _) = pure ()
analyzeExpr (U64E _) = pure ()
analyzeExpr (U32E _) = pure ()
analyzeExpr (BoolE _) = pure ()
analyzeExpr (CharE _) = pure ()
analyzeExpr (StrE _) = pure ()
analyzeExpr (ArrayMemE _ e) = do
    t <- getType e
    when (t /= I64T) (throwError $ TypeError I64T t "array access expects an i64")
analyzeExpr (AppE name exprs) = do
    _ <- lookupStack name
    mapM_ analyzeExpr exprs
analyzeExpr (IdE var) = do
    _ <- lookupEnv var
    pure ()
analyzeExpr (BinOpE op e1 e2) =
    case op of
        Assign ->
            case e1 of
                IdE n -> do
                    t1 <- lookupEnv n
                    t2 <- getType e2
                    if t1 /= t2
                        then throwError $ TypeError t1 t2 "assignment expects type of variable to be the same as value"
                        else do
                            sl <- getScope
                            updateVar (ScopeInfo sl (n, t1))
                ArrayMemE a _ -> do
                    at <- lookupEnv a
                    case at of
                        (ArrayT t1) -> do
                            t2 <- getType e2
                            if t1 /= t2
                                then throwError $ TypeError t1 t2 "assignment expects type of variable to be the same as value"
                                else do
                                    sl <- getScope
                                    updateVar (ScopeInfo sl (a, ArrayT t1))
                        _ -> throwError $ GeneralError "failed to store type array correctly"
                _ -> throwError $ GeneralError "assigment expects a variable and an expression"
        _ -> do
            t1 <- getType e1
            t2 <- getType e2
            _ <- binaryAnalysis op t1 t2
            pure ()
analyzeExpr (UnaryOpE op e) = do
    t <- getType e
    _ <- unaryAnalysis op t
    pure ()
analyzeExpr (IfE cndE thns elsifs elses) = do
    tcnd <- getType cndE
    when (tcnd /= BoolT) (throwError $ TypeError BoolT tcnd "if expects a bool in the conditional")
    tthen <- getType $ last thns
    if null elsifs
        then do
            if null elses
                then pure ()
                else do
                    telse <- getType $ last elses
                    if tthen == telse
                        then pure ()
                        else throwError $ TypeError tthen telse "if expressions expects same return type"
        else do
            let cnds = map fst elsifs
            types <- mapM getType cnds
            if all (== BoolT) types
                then do
                    telsif <- getType $ last $ concatMap snd elsifs
                    telse <- if null elses then pure tthen else getType $ last elses
                    if tthen == telse
                        then
                            if tthen == telsif
                                then pure ()
                                else throwError $ TypeError tthen telsif "| clause has different return type than if's return type"
                        else throwError $ TypeError tthen telse "else clause has different return type than if's return type"
                else throwError $ TypeError (head types) BoolT "|| expects bool on conditional"
analyzeExpr (DoE cndE exprs) = do
    tcnd <- getType cndE
    if tcnd /= BoolT
        then throwError $ TypeError BoolT tcnd "do conditional expects bool"
        else mapM_ analyzeExpr exprs

getType :: Expr -> SemanticsM Type
getType (IdE n) = lookupEnv n
getType (I64E _) = pure I64T
getType (I32E _) = pure I32T
getType (U64E _) = pure U64T
getType (U32E _) = pure U32T
getType (F64E _) = pure F64T
getType (F32E _) = pure F32T
getType (CharE _) = pure CharT
getType (StrE _) = pure StrT
getType (BoolE _) = pure BoolT
getType (BinOpE op e1 e2) = do
    t1 <- getType e1
    t2 <- getType e2
    binaryAnalysis op t1 t2
getType (UnaryOpE op e) = do
    t <- getType e
    unaryAnalysis op t
getType (IfE _ thns _ _) = getType $ last thns
getType (DoE _ exprs) = getType $ last exprs
getType (ArrayMemE a _) = do
    t <- lookupEnv a
    case t of
        (ArrayT at) -> pure at
        _ -> throwError $ GeneralError "failed to store type array correctly"
getType (AppE f _) = do
    b <- NE.last . body <$> lookupStack f
    getType b

binaryAnalysis :: BinOp -> (Type -> Type -> SemanticsM Type)
binaryAnalysis Add = add'
binaryAnalysis Sub = sub'
binaryAnalysis Mul = mul'
binaryAnalysis Div = div'
binaryAnalysis Mod = mod''
binaryAnalysis Exp = exp'
binaryAnalysis And = and'
binaryAnalysis XOr = xor'
binaryAnalysis Or = or'
binaryAnalysis Impl = impl'
binaryAnalysis Gt = gt'
binaryAnalysis GtEq = gte'
binaryAnalysis Lt = lt'
binaryAnalysis LtEq = lte'
binaryAnalysis Assign = assign'
binaryAnalysis Equal = eq'
binaryAnalysis NotEqual = neq'

unaryAnalysis :: UnOp -> (Type -> SemanticsM Type)
unaryAnalysis Not = not'
unaryAnalysis ArrLen = arrLen'

not' :: Type -> SemanticsM Type
not' BoolT = pure BoolT
not' t = throwError $ TypeError BoolT t "~ expects a bool"

arrLen' :: Type -> SemanticsM Type
arrLen' (ArrayT _) = pure I64T
arrLen' t = throwError $ TypeError (ArrayT I64T) t "# expects an array"

assign' :: Type -> Type -> SemanticsM Type
assign' t1 _ = pure t1

add' :: Type -> Type -> SemanticsM Type
add' I64T I64T = pure I64T
add' I32T I32T = pure I32T
add' U64T U64T = pure U64T
add' U32T U32T = pure U32T
add' F64T F64T = pure F64T
add' F32T F32T = pure F32T
add' t1 t2 = throwError $ TypeError t1 t2 "+ expects both to be numbers"

sub' :: Type -> Type -> SemanticsM Type
sub' I64T I64T = pure I64T
sub' I32T I32T = pure I32T
sub' U64T U64T = pure U64T
sub' U32T U32T = pure U32T
sub' F64T F64T = pure F64T
sub' F32T F32T = pure F32T
sub' t1 t2 = throwError $ TypeError t1 t2 "- expects both to be numbers"

mul' :: Type -> Type -> SemanticsM Type
mul' I64T I64T = pure I64T
mul' I32T I32T = pure I32T
mul' U64T U64T = pure U64T
mul' U32T U32T = pure U32T
mul' F64T F64T = pure F64T
mul' F32T F32T = pure F32T
mul' t1 t2 = throwError $ TypeError t1 t2 "* expects both to be numbers"

div' :: Type -> Type -> SemanticsM Type
div' I64T I64T = pure I64T
div' I32T I32T = pure I32T
div' U64T U64T = pure U64T
div' U32T U32T = pure U32T
div' F64T F64T = pure F64T
div' F32T F32T = pure F32T
div' t1 t2 = throwError $ TypeError t1 t2 "/ expects both to be numbers"

exp' :: Type -> Type -> SemanticsM Type
exp' I64T I64T = pure I64T
exp' I32T I32T = pure I32T
exp' U64T U64T = pure U64T
exp' U32T U32T = pure U32T
exp' F64T F64T = pure F64T
exp' F32T F32T = pure F32T
exp' t1 t2 = throwError $ TypeError t1 t2 "** expects both to be numbers"

mod'' :: Type -> Type -> SemanticsM Type
mod'' I64T I64T = pure I64T
mod'' I32T I32T = pure I32T
mod'' U64T U64T = pure U64T
mod'' U32T U32T = pure U32T
mod'' F64T F64T = pure F64T
mod'' t1 t2 = throwError $ TypeError t1 t2 "% expects both to be numbers"

and' :: Type -> Type -> SemanticsM Type
and' BoolT BoolT = pure BoolT
and' t1 t2 = throwError $ TypeError t1 t2 "/\\ expects both to be bool"

or' :: Type -> Type -> SemanticsM Type
or' BoolT BoolT = pure BoolT
or' t1 t2 = throwError $ TypeError t1 t2 "\\/ expects both to be bool"

xor' :: Type -> Type -> SemanticsM Type
xor' BoolT BoolT = pure BoolT
xor' t1 t2 = throwError $ TypeError t1 t2 "^ expects both to be bool"

impl' :: Type -> Type -> SemanticsM Type
impl' BoolT BoolT = pure BoolT
impl' t1 t2 = throwError $ TypeError t1 t2 "^ expects both to be bool"

gt' :: Type -> Type -> SemanticsM Type
gt' I64T I64T = pure BoolT
gt' I32T I32T = pure BoolT
gt' U64T U64T = pure BoolT
gt' U32T U32T = pure BoolT
gt' F64T F64T = pure BoolT
gt' F32T F32T = pure BoolT
gt' t1 t2 = throwError $ TypeError t1 t2 "> expects both to be numbers"

gte' :: Type -> Type -> SemanticsM Type
gte' I64T I64T = pure BoolT
gte' I32T I32T = pure BoolT
gte' U64T U64T = pure BoolT
gte' U32T U32T = pure BoolT
gte' F64T F64T = pure BoolT
gte' F32T F32T = pure BoolT
gte' t1 t2 = throwError $ TypeError t1 t2 ">= expects both to be numbers"

lt' :: Type -> Type -> SemanticsM Type
lt' I64T I64T = pure BoolT
lt' I32T I32T = pure BoolT
lt' U64T U64T = pure BoolT
lt' U32T U32T = pure BoolT
lt' F64T F64T = pure BoolT
lt' F32T F32T = pure BoolT
lt' t1 t2 = throwError $ TypeError t1 t2 "< expects both to be numbers"

lte' :: Type -> Type -> SemanticsM Type
lte' I64T I64T = pure BoolT
lte' I32T I32T = pure BoolT
lte' U64T U64T = pure BoolT
lte' U32T U32T = pure BoolT
lte' F64T F64T = pure BoolT
lte' F32T F32T = pure BoolT
lte' t1 t2 = throwError $ TypeError t1 t2 "<= expects both to be numbers"

eq' :: Type -> Type -> SemanticsM Type
eq' I64T I64T = pure BoolT
eq' I32T I32T = pure BoolT
eq' U64T U64T = pure BoolT
eq' U32T U32T = pure BoolT
eq' F64T F64T = pure BoolT
eq' F32T F32T = pure BoolT
eq' BoolT BoolT = pure BoolT
eq' StrT StrT = pure BoolT
eq' t1 t2 = throwError $ TypeError t1 t2 "= expects both expressions to be same type"

neq' :: Type -> Type -> SemanticsM Type
neq' I64T I64T = pure BoolT
neq' I32T I32T = pure BoolT
neq' U64T U64T = pure BoolT
neq' U32T U32T = pure BoolT
neq' F64T F64T = pure BoolT
neq' F32T F32T = pure BoolT
neq' BoolT BoolT = pure BoolT
neq' StrT StrT = pure BoolT
neq' t1 t2 = throwError $ TypeError t1 t2 "!= expects both expressions to be the same type"
