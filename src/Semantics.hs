module Semantics (
    analyzeProgram, 
    initStack,
    semanticsEval, 
) where

import Error
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Expressions
import SemanticsMonad

analyzeProgram :: Program -> SemanticsM ()
analyzeProgram prog = analyzeProgramIter prog 
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
    mapM_ (\(v, t) -> updateEnv (ScopeInfo lvl (v, (None, t)))) params
    mapM_ (\(v, t) -> updateEnv (ScopeInfo lvl (v, (None, t)))) locals
    mapM_ analyzeExpr exprs

analyzeExpr :: Expr -> SemanticsM ()
analyzeExpr (I64E _) = pure ()
analyzeExpr (I32E _) = pure ()
analyzeExpr (F64E _) = pure ()
analyzeExpr (F32E _) = pure ()
analyzeExpr (U64E _) = pure ()
analyzeExpr (U32E _) = pure ()
analyzeExpr (BoolE _) = pure ()
analyzeExpr (StrE _) = pure ()
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
                    t1 <- snd <$> lookupEnv n
                    t2 <- getType e2
                    if t1 /= t2
                        then throwError $ TypeError t1 t2
                        else do
                            val <- getValue e2
                            sl <- getScope 
                            updateVar (ScopeInfo sl (n, (val, t1)))
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
    when (tcnd /= BoolT) (throwError $ GeneralError "if expects boolean")
    tthen <- getType $ last thns
    if null elsifs 
        then do
            when (null elses) (pure ())
            telse <- getType $ last elses
            if tthen == telse
                then pure ()
                else throwError $ GeneralError "if expressions expect same return type"
        else do
            let cnds = map fst elsifs 
            types <- mapM getType cnds
            if all (== BoolT) types
                then do
                telsif <- getType $ last $ concat $ map snd elsifs
                telse <- getType $ last elses
                if tthen == telse && tthen == telsif
                    then pure ()
                    else throwError $ GeneralError "if expressions expect same return type"
                else throwError $ GeneralError "|| expects boolean"
analyzeExpr (DoE cndE exprs) = do
    tcnd <- getType cndE
    if tcnd /= BoolT
        then throwError $ GeneralError "do expects boolean"
        else mapM_ analyzeExpr exprs
analyzeExpr _ = undefined

getValue :: Expr -> SemanticsM Value
getValue (I64E n) = pure $ I64V n
getValue (I32E n) = pure $ I32V n
getValue (U64E n) = pure $ U64V n
getValue (U32E n) = pure $ U32V n
getValue (F64E n) = pure $ F64V n
getValue (F32E n) = pure $ F32V n
getValue (CharE c) = pure $ CharV c
getValue (StrE s) = pure $ StrV s
getValue (BoolE b) = pure $ BoolV b
getValue (IdE n) = do
    (val, _) <- lookupEnv n
    pure val
getValue _ = undefined

getType :: Expr -> SemanticsM Type
getType (IdE n) = snd <$> lookupEnv n
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
getType (ArrayMemE a _) = snd <$> lookupEnv a 
getType _ = undefined
    
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
binaryAnalysis _ = undefined
{-binaryAnalysis Equal = eq'
binaryAnalysis NotEqual = neq'
-}

unaryAnalysis :: UnOp -> (Type -> SemanticsM Type)
unaryAnalysis Not = not'
unaryAnalysis ArrLen = arrLen'

not' :: Type -> SemanticsM Type
not' BoolT = pure BoolT
not' _ = throwError $ GeneralError "~ expects a boolean"

arrLen' :: Type -> SemanticsM Type
arrLen' (ArrayT _) = pure U64T
arrLen' _ = throwError $ GeneralError "# expects an array"

assign' :: Type -> Type -> SemanticsM Type
assign' t1 _ = pure t1

add' :: Type -> Type -> SemanticsM Type
add' I64T I64T = pure I64T
add' I32T I32T = pure I32T
add' U64T U64T = pure U64T
add' U32T U32T = pure U32T
add' F64T F64T = pure F64T
add' F32T F32T = pure F32T
add' _ _ = throwError $ GeneralError "+ expects number"

sub' :: Type -> Type -> SemanticsM Type
sub' I64T I64T = pure I64T
sub' I32T I32T = pure I32T
sub' U64T U64T = pure U64T
sub' U32T U32T = pure U32T
sub' F64T F64T = pure F64T
sub' F32T F32T = pure F32T
sub' _ _ = throwError $ GeneralError "- expects number"

mul' :: Type -> Type -> SemanticsM Type
mul' I64T I64T = pure I64T
mul' I32T I32T = pure I32T
mul' U64T U64T = pure U64T
mul' U32T U32T = pure U32T
mul' F64T F64T = pure F64T
mul' F32T F32T = pure F32T
mul' _ _ = throwError $ GeneralError "* expects number"

div' :: Type -> Type -> SemanticsM Type
div' I64T I64T = pure I64T
div' I32T I32T = pure I32T
div' U64T U64T = pure U64T
div' U32T U32T = pure U32T
div' F64T F64T = pure F64T
div' F32T F32T = pure F32T
div' _ _ = throwError $ GeneralError "/ expects number"

exp' :: Type -> Type -> SemanticsM Type
exp' I64T I64T = pure I64T
exp' I32T I32T = pure I32T
exp' U64T U64T = pure U64T
exp' U32T U32T = pure U32T
exp' F64T F64T = pure F64T
exp' F32T F32T = pure F32T
exp' _ _ = throwError $ GeneralError "** expects number"

mod'' :: Type -> Type -> SemanticsM Type
mod'' I64T I64T = pure I64T
mod'' I32T I32T = pure I32T
mod'' U64T U64T = pure U64T
mod'' U32T U32T = pure U32T
mod'' F64T F64T = pure F64T
mod'' _ _ = throwError $ GeneralError "% expects number"

and' :: Type -> Type -> SemanticsM Type
and' BoolT BoolT = pure BoolT
and' _ _ = throwError $ GeneralError "/\\ expects boolean"

or' :: Type -> Type -> SemanticsM Type
or' BoolT BoolT = pure BoolT
or' _ _ = throwError $ GeneralError "\\/ expects boolean"

xor' :: Type -> Type -> SemanticsM Type
xor' BoolT BoolT = pure BoolT
xor' _ _ = throwError $ GeneralError "^ expects boolean"

impl' :: Type -> Type -> SemanticsM Type
impl' BoolT BoolT = pure BoolT
impl' _ _ = throwError $ GeneralError "^ expects boolean"

gt' :: Type -> Type -> SemanticsM Type
gt' I64T I64T = pure BoolT
gt' I32T I32T = pure BoolT
gt' U64T U64T = pure BoolT
gt' U32T U32T = pure BoolT
gt' F64T F64T = pure BoolT
gt' F32T F32T = pure BoolT
gt' _ _ = throwError $ GeneralError "> expects number"

gte' :: Type -> Type -> SemanticsM Type
gte' I64T I64T = pure BoolT
gte' I32T I32T = pure BoolT
gte' U64T U64T = pure BoolT
gte' U32T U32T = pure BoolT
gte' F64T F64T = pure BoolT
gte' F32T F32T = pure BoolT
gte' _ _ = throwError $ GeneralError ">= expects number"

lt' :: Type -> Type -> SemanticsM Type
lt' I64T I64T = pure BoolT
lt' I32T I32T = pure BoolT
lt' U64T U64T = pure BoolT
lt' U32T U32T = pure BoolT
lt' F64T F64T = pure BoolT
lt' F32T F32T = pure BoolT
lt' _ _ = throwError $ GeneralError "< expects number"

lte' :: Type -> Type -> SemanticsM Type
lte' I64T I64T = pure BoolT
lte' I32T I32T = pure BoolT
lte' U64T U64T = pure BoolT
lte' U32T U32T = pure BoolT
lte' F64T F64T = pure BoolT
lte' F32T F32T = pure BoolT
lte' _ _ = throwError $ GeneralError "<= expects number"

{-
eq' :: Expr -> Expr -> SemanticsM ()
eq' (I64E _) (I64E _) = pure ()
eq' (I32E _) (I32E _) = pure ()
eq' (U64E _) (U64E _) = pure ()
eq' (U32E _) (U32E _) = pure ()
eq' (F64E _) (F64E _) = pure ()
eq' (F32E _) (F32E _) = pure ()
eq' (BoolE _) (BoolE _) = pure ()
eq' (StrE _) (StrE _) = pure ()
eq' _ _ = throwError $ GeneralError "= expects number"

neq' :: Type -> Type -> SemanticsM ()
neq' I64T I64T = pure ()
neq' (I32E _) (I32E _) = pure ()
neq' (U64E _) (U64E _) = pure ()
neq' (U32E _) (U32E _) = pure ()
neq' (F64E _) (F64E _) = pure ()
neq' (F32E _) (F32E _) = pure ()
neq' (BoolE _) (BoolE _) = pure ()
neq' (StrE _) (StrE _) = pure ()
neq' _ _ = throwError $ GeneralError "!= types do not match"
-}
