{-# LANGUAGE FlexibleInstances    #-}

module TypeChecker.TypeChecker where
import TypeChecker.MonadUtils
import TypeChecker.Environment
import Parser.Abs
import Control.Monad.Except
import Control.Monad.State
import TypeChecker.Utils
import Data.Maybe

checkTypes :: Program -> Either TypeCheckException TypeName
checkTypes program =
    runExcept $ evalStateT (checkTypesM program) emptyEnv

class TypeChecker a where
    checkTypesM :: a -> TypeCheckerM

instance TypeChecker Program where
    checkTypesM (Program _ inits) = do
        void $ checkInitNamesTopDef inits
        mapM_ checkTypesM inits
        env <- get
        unless (checkMain env) $ throwError NoMainException
        return TVoid

checkFunction :: BNFC'Position -> Type -> Ident -> [Arg] -> Block -> TypeCheckerM
checkFunction position retType (Ident name) args block = do
    env1 <- get
    void $ checkFunctionArguments args
    let retTypeName = getNameType retType
    put $ addFunction env1 retTypeName (Ident name) args
    envWithoutArgs <- get
    put $ addArguments envWithoutArgs args
    env2 <- get
    put $ addReturnType env2 retTypeName
    void $ checkTypesM block
    env3 <- get
    unless (retTypeName == TVoid || wasReturn env3) $ throwError $ NoReturnException name position
    put envWithoutArgs
    return TVoid

checkVariables :: BNFC'Position -> Type -> [Item] -> TypeCheckerM
checkVariables position type_ inits = do
    let typeName = getNameType type_
    when (typeName == TVoid) $ throwError $ VoidTypeVariableException position
    let names = concatMap getInitNamesItem inits
    mapM_ (`addVariable` typeName) names
    let expressions = map getExpressionFromItem inits
    let expressionsFiltered = map fromJust $ filter (/= Nothing) expressions
    checkExpressions expressionsFiltered typeName

instance TypeChecker TopDef where
    checkTypesM (FnDef position retType (Ident name) args block) =
        checkFunction position retType (Ident name) args block
    checkTypesM (VarDef position type_ inits) =
        checkVariables position type_ inits

instance TypeChecker Block where
    checkTypesM (Block _ stmts) = do
        void $ checkInitNamesBlock stmts
        let returnedType = TVoid
        mapM_ checkTypesM stmts
        return returnedType

instance TypeChecker Stmt where
    checkTypesM (Empty _) = return TVoid
    checkTypesM (BStmt _ block) = do
        envBeforeBlock <- get
        void $ checkTypesM block
        put envBeforeBlock
        return TVoid
    checkTypesM (FnDecl position type_ (Ident name) args block) =
        checkFunction position type_ (Ident name) args block
    checkTypesM (VarDecl position type_ inits) = 
        checkVariables position type_ inits
    checkTypesM (Ass position (Ident name) expression) = do
        expType <- checkTypesM expression
        env <- get
        case getType env (Ident name) of
            Nothing -> throwError $ VariableNotFoundException name position
            Just varType -> assertEqualTypes varType expType position
    checkTypesM (Incr position (Ident name)) = do
        env <- get
        case getType env (Ident name) of
            Nothing -> throwError $ VariableNotFoundException name position
            Just varType -> assertEqualTypes TInt varType position
    checkTypesM (Decr position (Ident name)) = do
        env <- get
        case getType env (Ident name) of
            Nothing -> throwError $ VariableNotFoundException name position
            Just varType -> assertEqualTypes TInt varType position
    checkTypesM (Ret position expression) = do
        expType <- checkTypesM expression
        env <- get
        case returnType env of
            Nothing -> throwError $ ReturnOutsideFunctionException position
            Just retType -> do
                unless (retType == expType) $ throwError $ ReturnMismatchException retType expType position
                put $ env { wasReturn = True }
        return TVoid
    checkTypesM (VRet position) = do
        env <- get
        case returnType env of
            Nothing -> throwError $ ReturnOutsideFunctionException position
            Just retType -> unless (retType == TVoid) $ throwError $ ReturnMismatchException retType TVoid position
        return TVoid
    checkTypesM (Cond position expression stmt) = do
        expType <- checkTypesM expression
        void $ assertEqualTypes TBool expType position
        checkTypesM stmt
    checkTypesM (CondElse position expression stmt1 stmt2) = do
        expType <- checkTypesM expression
        void $ assertEqualTypes TBool expType position
        void $ checkTypesM stmt1
        checkTypesM stmt2
    checkTypesM (While position expression stmt) = do
        expType <- checkTypesM expression
        unless (expType == TBool) $ throwError $ TypeMismatchException TBool expType position
        checkTypesM stmt
    checkTypesM (SExp _ expression) = checkTypesM expression

checkExpressions :: [Expr] -> TypeName -> TypeCheckerM
checkExpressions (expr:xs) expectedType = do
    typeName <- checkTypesM expr
    void $ assertEqualTypes expectedType typeName (getPositionFromExpr expr)
    checkExpressions xs expectedType
checkExpressions [] _ = return TVoid

instance TypeChecker Expr where
    checkTypesM (EVar position (Ident name)) = do
        env <- get
        case getType env (Ident name) of
            Just typeName -> return typeName
            Nothing -> throwError $ VariableNotFoundException name position
    checkTypesM (ELitInt _ _) = return TInt
    checkTypesM (ELitTrue _) = return TBool
    checkTypesM (ELitFalse _) = return TBool
    checkTypesM (EString _ _) = return TStr
    checkTypesM (Neg position expr) = do
        typeName <- checkTypesM expr
        case typeName of
            TInt -> return TInt
            _ -> throwError $ TypeMismatchException TInt typeName position
    checkTypesM (Not position expr) = do
        typeName <- checkTypesM expr
        case typeName of
            TBool -> return TBool
            _ -> throwError $ TypeMismatchException TBool typeName position
    checkTypesM (EApp position (Ident name) args) = do
        env <- get
        case getFunction env (Ident name) of
            Just (retType, params) -> do
                let paramTypes = map getNameArg params
                let argTypesM = map checkTypesM args
                argTypes <- sequence argTypesM
                unless (argTypes == paramTypes) $
                    throwError $ 
                        InvalidFunctionCallException name position paramTypes argTypes
                unless (areRefsIdents params args) $ 
                    throwError $ NoIdentPassedAsRefException name position
                return retType
            Nothing -> throwError $ FunctionNotFoundException name position
    checkTypesM (EMul position expr1 _ expr2) = do
        typeName1 <- checkTypesM expr1
        typeName2 <- checkTypesM expr2
        case (typeName1, typeName2) of
            (TInt, TInt) -> return TInt
            (TInt, _) -> throwError $ TypeMismatchException TInt typeName2 position
            _ -> throwError $ TypeMismatchException TInt typeName1 position
    checkTypesM (EAdd position expr1 op expr2) = do
        typeName1 <- checkTypesM expr1
        typeName2 <- checkTypesM expr2
        case (typeName1, typeName2) of
            (TInt, TInt) -> return TInt
            (TInt, _) -> throwError $ TypeMismatchException TInt typeName2 position
            (TStr, TStr) -> case op of
                Plus _ -> return TStr
                _ -> throwError $ TypeMismatchException TInt typeName1 position
            (TStr, _) -> throwError $ TypeMismatchException TStr typeName2 position
            _ -> throwError $ TypeMismatchException TInt typeName1 position
    checkTypesM (ERel position expr1 op expr2) = do
        typeName1 <- checkTypesM expr1
        typeName2 <- checkTypesM expr2
        case (typeName1, typeName2) of
            (TInt, TInt) -> return TBool
            (TInt, _) -> throwError $ TypeMismatchException TInt typeName2 position
            (TStr, TStr) -> case op of
                EQU _ -> return TBool
                NE _ -> return TBool
                _ -> throwError $ TypeMismatchException TInt typeName1 position
            (TStr, _) -> throwError $ TypeMismatchException TStr typeName2 position
            (TBool, TBool) -> case op of
                EQU _ -> return TBool
                NE _ -> return TBool
                _ -> throwError $ TypeMismatchException TInt typeName1 position
            (TBool, _) -> throwError $ TypeMismatchException TBool typeName2 position
            _ -> throwError $ TypeMismatchException TInt typeName1 position
    checkTypesM (EAnd position expr1 expr2) = do
        typeName1 <- checkTypesM expr1
        typeName2 <- checkTypesM expr2
        case (typeName1, typeName2) of
            (TBool, TBool) -> return TBool
            (TBool, _) -> throwError $ TypeMismatchException TBool typeName2 position
            _ -> throwError $ TypeMismatchException TBool typeName1 position
    checkTypesM (EOr position expr1 expr2) = do
        typeName1 <- checkTypesM expr1
        typeName2 <- checkTypesM expr2
        case (typeName1, typeName2) of
            (TBool, TBool) -> return TBool
            (TBool, _) -> throwError $ TypeMismatchException TBool typeName2 position
            _ -> throwError $ TypeMismatchException TBool typeName1 position