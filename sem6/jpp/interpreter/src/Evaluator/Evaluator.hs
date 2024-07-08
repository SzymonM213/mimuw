{-# LANGUAGE FlexibleInstances    #-}

module Evaluator.Evaluator where
import Parser.Abs
import Evaluator.Configuration
import Evaluator.Utils
import Control.Monad.Except
import Control.Monad.State

type EvaluatorM' a = StateT Conf (ExceptT EvaluateException IO) a
type EvaluatorM = EvaluatorM' Value

class Evaluator a where 
    evalM :: a -> EvaluatorM

eval :: Program -> IO (Either EvaluateException Value)
eval program = do
    runExceptT $ evalStateT (evalM program) emptyConf

instance Evaluator Program where
    evalM (Program _ topDefs) = do
        mapM_ evalM topDefs
        conf <- get
        return $ getReturnVal conf

putNewItems :: Conf -> Type -> [Item] -> EvaluatorM
putNewItems _ type_ ((Init _ name expr):xs) = do
    evaluatedExpr <- evalM expr
    confAfterExpr <- get
    let newConf = putNewValue confAfterExpr name evaluatedExpr
    put newConf
    putNewItems newConf type_ xs
putNewItems _ type_ ((NoInit _ name):xs) = do
    conf <- get
    let newConf = putNewValue conf name (defaultValue type_)
    put newConf
    putNewItems newConf type_ xs
putNewItems _ _ [] = return VVoid

instance Evaluator TopDef where
    evalM (FnDef _ type_ (Ident name) args block) = do
        conf <- get
        let funType = VFun (isVoid type_) args block (env conf)
        put $ putNewValue conf (Ident name) funType
        if name == "main"
            then evalM block
            else return VVoid
    evalM (VarDef _ type_ items) = do
        conf <- get
        void $ putNewItems conf type_ items
        return VVoid

instance Evaluator Block where
    evalM (Block _ stmts) = do
        mapM_ evalM stmts
        return VVoid

evalAndRetrieveEnv :: EvaluatorM -> EvaluatorM
evalAndRetrieveEnv m = do
    conf1 <- get
    let envBefore = env conf1
    void m
    conf2 <- get
    if returned conf2
        then return VVoid
        else do
            put $ Conf envBefore (store conf2) (next conf2)
            return VVoid

instance Evaluator Stmt where
    evalM (Empty _) = return VVoid
    evalM (BStmt _ block) = do
        evalAndRetrieveEnv $ evalM block
    evalM (FnDecl _ type_ (Ident name) args block) = do
        conf <- get
        unless (returned conf) $ do
            let funType = VFun (isVoid type_) args block (env conf)
            put $ putNewValue conf (Ident name) funType
        return VVoid
    evalM (VarDecl _ type_ items) = do
        conf <- get
        unless (returned conf) $ do
            void $ putNewItems conf type_ items
        return VVoid
    evalM (Ass _ (Ident name) expression) = do
        conf <- get
        unless (returned conf) $ do
            expressionResult <- evalM expression
            confAfterExpr <- get
            put $ putValue confAfterExpr (Ident name) expressionResult
        return VVoid
    evalM (Incr _ (Ident name)) = do
        conf <- get
        unless (returned conf) $ do
            let v = getValue conf (Ident name)
            case v of
                (VInt i) -> put $ putValue conf (Ident name) (VInt (i + 1))
                _ -> throwError UnexpectedException
        return VVoid
    evalM (Decr _ (Ident name)) = do
        conf <- get
        unless (returned conf) $ do
            let v = getValue conf (Ident name)
            case v of
                (VInt i) -> put $ putValue conf (Ident name) (VInt (i - 1))
                _ -> throwError UnexpectedException
        return VVoid
    evalM (Ret _ expression) = do
        conf <- get
        unless (returned conf) $ do
            expressionResult <- evalM expression
            confAfterExpr <- get
            put $ putValue confAfterExpr (Ident "return") expressionResult
        return VVoid
    evalM (VRet _) = do
        conf <- get
        unless (returned conf) $ do
            put $ putValue conf (Ident "return") VVoid
        return VVoid
    evalM (Cond _ expr block) = do
        conf <- get
        unless (returned conf) $ do
            VBool b <- evalM expr
            void $ evalAndRetrieveEnv $ if b then evalM block else return VVoid
        return VVoid
    evalM (CondElse _ expr block1 block2) = do
        conf <- get
        unless (returned conf) $ do
            VBool b <- evalM expr
            void $ evalAndRetrieveEnv $ if b then evalM block1 else evalM block2
        return VVoid
    evalM (While position expr block) = do
        conf <- get
        unless (returned conf) $ do
            VBool b <- evalM expr
            void $ evalAndRetrieveEnv $ 
                if b then evalM block >> evalM (While position expr block) 
                else return VVoid
        return VVoid
    evalM (SExp _ expr) = do
        conf <- get
        unless (returned conf) $ do
            void $ evalM expr
        return VVoid

handlePrint :: Value -> EvaluatorM
handlePrint v = do
    liftIO $ print v
    return VVoid

passArguments :: Conf -> Ident -> [Arg] -> [(Value, Maybe Ident)] -> EvaluatorM
passArguments _ _ [] [] = return VVoid
passArguments envOutside (Ident fnName) ((ArgVal _ _ (Ident name)):args) (v:valuesList) = do
    case v of
        (value, _) -> do
            conf <- get
            let newConf = putValue conf (Ident name) value
            put newConf
            passArguments envOutside (Ident fnName) args valuesList
passArguments envOutside (Ident fnName) ((ArgRef _ _ (Ident paramName)):args) (v:valuesList) = do
    case v of
        (_, Just (Ident varName)) -> do
            conf <- get
            let newConf = addRef conf envOutside (Ident paramName) (Ident varName)
            put newConf
            passArguments envOutside (Ident fnName) args valuesList
        (_, Nothing) -> throwError UnexpectedException
passArguments _ _ _ _ = throwError UnexpectedException

handlePrintFunction :: [Expr] -> EvaluatorM
handlePrintFunction exprs = do
    value <- evalM (head exprs)
    void $ handlePrint value
    return VVoid

putAndUpdate :: Conf -> (Conf -> Conf) -> EvaluatorM' Conf
putAndUpdate conf f = do
    let newConf = f conf
    put newConf
    return newConf

extractIdent :: Expr -> EvaluatorM' (Value, Maybe Ident)
extractIdent expr = case expr of
    EVar _ ident -> do
        conf <- get
        return (getValue conf ident, Just ident)
    _ -> do
        value <- evalM expr
        return (value, Nothing)

instance Evaluator Expr where
    evalM (EVar _ ident) = do
        conf <- get
        return $ getValue conf ident
    evalM (ELitInt _ int) = return $ VInt int
    evalM (ELitTrue _) = return $ VBool True
    evalM (ELitFalse _) = return $ VBool False
    evalM (EApp position (Ident name) exprs) = do
        if isPrint (Ident name) then handlePrintFunction exprs
        else do
            confOutsideFunction <- get
            let fun = getValue confOutsideFunction (Ident name)
            case fun of
                VFun isVoid_ args block environment -> do
                    argValuesWithNames <- mapM extractIdent exprs
                    confWithFuncEnv <- putAndUpdate confOutsideFunction (setEnv environment)
                    void $ putAndUpdate confWithFuncEnv (
                        addFunctionToAllowRecursion (Ident name) confOutsideFunction)
                    void $ passArguments confOutsideFunction (Ident name) 
                        args argValuesWithNames
                    void $ evalM block
                    confAfterFunction <- get
                    unless (returned confAfterFunction || isVoid_) $ 
                        throwError $ NoReturnException name position
                    let returnVal = getReturnVal confAfterFunction
                    put $ Conf (env confOutsideFunction) 
                        (store confAfterFunction) (next confOutsideFunction)
                    return returnVal
                _ -> throwError $ InvalidReferenceException name position
    evalM (EString _ str) = return $ VStr str
    evalM (Neg _ expr) = do
        v <- evalM expr
        case v of
            VInt i -> return $ VInt (-i)
            _ -> throwError UnexpectedException
    evalM (Not _ expr) = do
        v <- evalM expr
        case v of
            VBool b -> return $ VBool (not b)
            _ -> throwError UnexpectedException
    evalM (EMul pos expr1 op expr2) = do
        v1 <- evalM expr1
        v2 <- evalM expr2
        case (v1, v2) of
            (VInt i1, VInt i2) -> case op of
                Times _ -> return $ VInt (i1 * i2)
                Div _ -> if i2 == 0 then throwError (DivByZeroException pos) 
                    else return $ VInt (i1 `div` i2)
                Mod _ -> if i2 == 0 then throwError (DivByZeroException pos) 
                    else return $ VInt (i1 `mod` i2)
            _ -> throwError UnexpectedException
    evalM (EAdd _ expr1 op expr2) = do
        v1 <- evalM expr1
        v2 <- evalM expr2
        case (v1, v2) of
            (VStr s1, VStr s2) -> case op of
                Plus _ -> return $ VStr (s1 ++ s2)
                _ -> throwError UnexpectedException
            (VInt i1, VInt i2) -> case op of
                Plus _ -> return $ VInt (i1 + i2)
                Minus _ -> return $ VInt (i1 - i2)
            _ -> throwError UnexpectedException
    evalM (ERel _ expr1 op expr2) = do
        v1 <- evalM expr1
        v2 <- evalM expr2
        case (v1, v2) of
            (VInt i1, VInt i2) -> case op of
                LTH _ -> return $ VBool (i1 < i2)
                LE _ -> return $ VBool (i1 <= i2)
                GTH _ -> return $ VBool (i1 > i2)
                GE _ -> return $ VBool (i1 >= i2)
                EQU _ -> return $ VBool (i1 == i2)
                NE _ -> return $ VBool (i1 /= i2)
            (VBool b1, VBool b2) -> case op of
                EQU _ -> return $ VBool (b1 == b2)
                NE _ -> return $ VBool (b1 /= b2)
                _ -> throwError UnexpectedException
            (VStr s1, VStr s2) -> case op of
                EQU _ -> return $ VBool (s1 == s2)
                NE _ -> return $ VBool (s1 /= s2)
                _ -> throwError UnexpectedException
            _ -> throwError UnexpectedException
    evalM (EAnd _ expr1 expr2) = do
        v1 <- evalM expr1
        v2 <- evalM expr2
        case (v1, v2) of
            (VBool b1, VBool b2) -> return $ VBool (b1 && b2)
            _ -> throwError UnexpectedException
    evalM (EOr _ expr1 expr2) = do
        v1 <- evalM expr1
        v2 <- evalM expr2
        case (v1, v2) of
            (VBool b1, VBool b2) -> return $ VBool (b1 || b2)
            _ -> throwError UnexpectedException
