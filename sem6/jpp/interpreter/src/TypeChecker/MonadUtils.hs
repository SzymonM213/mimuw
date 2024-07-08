{-# LANGUAGE MultiParamTypeClasses #-}
module TypeChecker.MonadUtils where
import Control.Monad.Except
import Control.Monad.State
import TypeChecker.Environment
import Parser.Abs
import TypeChecker.Utils

type TypeCheckerM = StateT Env (Except TypeCheckException) TypeName

checkInitNamesTopDef :: [TopDef] -> TypeCheckerM
checkInitNamesTopDef topdefs = do
    let names = concatMap getInitNamesTopDef topdefs
    case getNamesDefinedMultipleTimes names of
        Nothing -> return TVoid
        Just name -> do
            let positions = concatMap (getPositionsOfDuplicatesTD name) topdefs
            let pos1 = head positions
            let pos2 = last positions
            throwError $ MultipleDefinitionException (show name) pos1 pos2

checkInitNamesBlock :: [Stmt] -> TypeCheckerM
checkInitNamesBlock stmts = do
    let names = concatMap getInitNamesStmt stmts
    case getNamesDefinedMultipleTimes names of
        Nothing -> return TVoid
        Just name -> do
            let positions = concatMap (getPositionsOfDuplicatesStmt name) stmts
            let pos1 = head positions
            let pos2 = last positions
            throwError $ MultipleDefinitionException (show name) pos1 pos2

assertEqualTypes :: TypeName -> TypeName -> BNFC'Position -> TypeCheckerM
assertEqualTypes expected actual position = do
    unless (expected == actual) $ do 
        throwError $ TypeMismatchException expected actual position
    return TVoid

checkFunctionArguments :: [Arg] -> TypeCheckerM
checkFunctionArguments args = do
    let names = concatMap getInitNamesArg args
    case getNamesDefinedMultipleTimes names of
        Nothing -> return TVoid
        Just (Ident name) -> do
            let positions = concatMap (getPositionsOfDuplicatesArg (Ident name)) args
            let pos1 = head positions
            let pos2 = last positions
            throwError $ MultipleDefinitionException name pos1 pos2

addVariable :: Ident -> TypeName -> TypeCheckerM
addVariable (Ident name) typeName = do
    env <- get
    put $ addType env (Ident name) typeName
    return TVoid