{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Evaluator.Configuration where
import Parser.Abs
import Data.Map
import Prelude

data Value = VInt Integer | VBool Bool | VStr String | VVoid | VFun Bool [Arg] Block Env

defaultValue :: Type -> Value
defaultValue (Int _) = VInt 0
defaultValue (Str _) = VStr ""
defaultValue (Bool _) = VBool False
defaultValue (Void _) = VVoid

instance Show Value where
    show (VInt i) = show i
    show (VBool b) = show b
    show (VStr s) = s
    show VVoid = "void"
    show (VFun {}) = "function"

data Store = Store {
    values :: Map Integer Value
}

emptyStore :: Store
emptyStore = Store {
    values = empty
}

data Env = Env {
    locations :: Map String Integer
}

emptyEnv :: Env
emptyEnv = Env {
    locations = empty
}

data Conf = Conf {
    env :: Env,
    store :: Store,
    next :: Integer
}

emptyConf :: Conf
emptyConf = Conf {
    env = emptyEnv,
    store = emptyStore,
    next = 0
}

putValueStore :: Conf -> String -> Value -> Conf
putValueStore conf name value = (conf {
    store = (store conf) {
        values = insert loc value (values (store conf))
    }
}) where loc = locations (env conf) ! name

putValueEnv :: Conf -> Ident -> (Conf, Integer)
putValueEnv conf (Ident name) =
    let newEnv = (env conf) {
        locations = insert name (next conf) (locations (env conf))
    }
    in (conf { env = newEnv , next = next conf + 1}, next conf)

putValue :: Conf -> Ident -> Value -> Conf
putValue conf (Ident name) value = do
    if member name (locations (env conf)) then
        putValueStore conf name value
    else
        let (newConf, _) = putValueEnv conf (Ident name)
        in putValueStore newConf name value

putNewValue :: Conf -> Ident -> Value -> Conf
putNewValue conf (Ident name) value = do
    let (newConf, _) = putValueEnv conf (Ident name)
    putValueStore newConf name value

putNewValues :: Conf -> [Ident] -> [Value] -> Conf
putNewValues conf [] [] = conf
putNewValues conf (name:names) (val:vals) = 
    putNewValues (putNewValue conf name val) names vals
putNewValues conf _ _ = conf

getValue :: Conf -> Ident -> Value
getValue conf (Ident name) = do
    let loc = locations (env conf) ! name
    values (store conf) ! loc

isValue :: Conf -> Ident -> Bool
isValue conf (Ident name) = member name (locations (env conf))

returned :: Conf -> Bool
returned conf = member "return" (locations (env conf))

getReturnVal :: Conf -> Value
getReturnVal conf = getValue conf (Ident "return")

addRef :: Conf -> Conf -> Ident -> Ident -> Conf
addRef conf confOutsideFunction (Ident name) (Ident ref) =
    let loc = locations (env confOutsideFunction) ! ref
    in conf {
        env = (env conf) {
            locations = insert name loc (locations (env conf))
        }
    }

leaveFunction :: Conf -> Conf -> Conf
leaveFunction conf confOutsideFunction = do
    let returnVal = getReturnVal conf
    putValue confOutsideFunction (Ident "return") returnVal

setEnv :: Env -> Conf -> Conf
setEnv newEnv conf = conf { env = newEnv }

addFunctionToAllowRecursion :: Ident -> Conf -> Conf ->  Conf
addFunctionToAllowRecursion (Ident name) confOutsideFunction confInside = do
    let loc = locations (env confOutsideFunction) ! name
    confInside {
        env = (env confInside) {
            locations = insert name loc (locations (env confInside))
        }
    }

instance Show Store where
    show (Store vals) = "Store: " ++ show vals

instance Show Env where
    show (Env locs) = "Env: " ++ show locs

instance Show Conf where
    show (Conf env_ store_ next_) = 
        "Conf: " ++ show env_ ++ " " ++ show store_ ++ " " ++ show next_