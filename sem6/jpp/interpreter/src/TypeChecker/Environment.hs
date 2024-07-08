module TypeChecker.Environment where
import Data.Map
import Parser.Abs
import Prelude
import TypeChecker.Utils

data Env = Env {
    types :: Map String TypeName,
    functions :: Map String (TypeName, [Arg]),
    returnType :: Maybe TypeName,
    wasReturn :: Bool
}

instance Show Env where
    show env = "Env { types = " ++ show (types env) ++ ", functions = "
               ++ show (functions env) ++ ", returnType = " 
               ++ show (returnType env) ++ " }"

emptyEnv :: Env
emptyEnv = Env {
    types = empty,
    functions = Data.Map.fromList [("printInt", (TVoid, [ArgVal Nothing 
                                    (Int Nothing) (Ident "x")])), 
                                   ("printBool", (TVoid, [ArgVal Nothing 
                                    (Bool Nothing) (Ident "x")])), 
                                   ("printString", (TVoid, [ArgVal Nothing 
                                    (Str Nothing) (Ident "x")]))],
    returnType = Nothing,
    wasReturn = False
}

addType :: Env -> Ident -> TypeName -> Env
addType env (Ident name) typeName = env { types = insert name typeName (types env) }

addFunction :: Env -> TypeName -> Ident -> [Arg] -> Env
addFunction env type_ (Ident name) args = do
    let newFunction = fromList [(name, (type_, args))]
    env { functions = Data.Map.union newFunction (functions env) }

addArguments :: Env -> [Arg] -> Env
addArguments env args = do
    let argIdents = concatMap getInitNamesArg args
    let argNames = Prelude.map (\(Ident name) -> name) argIdents
    let argTypes = concatMap getTypeNamesArg args
    let argTypesWithNames = zip argNames argTypes
    let newTypes = fromList argTypesWithNames
    env { types = Data.Map.union newTypes (types env) }

addReturnType :: Env -> TypeName -> Env
addReturnType env type_ = env { returnType = Just type_ , wasReturn = False }

getType :: Env -> Ident -> Maybe TypeName
getType env (Ident name) = Data.Map.lookup name (types env)

getFunction :: Env -> Ident -> Maybe (TypeName, [Arg])
getFunction env (Ident name) = Data.Map.lookup name (functions env)

checkMain :: Env -> Bool
checkMain env = case getFunction env (Ident "main") of
    Just (TInt, []) -> True
    _ -> False