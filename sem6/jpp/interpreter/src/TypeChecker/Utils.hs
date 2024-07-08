
module TypeChecker.Utils where
import Parser.Abs
import Data.List

data TypeName = TInt | TBool | TStr | TVoid
    deriving (Eq)

instance Show TypeName where
    show TInt = "Int"
    show TBool = "Bool"
    show TStr = "String"
    show TVoid = "Void"

getNameType :: Type -> TypeName
getNameType (Int _) = TInt
getNameType (Bool _) = TBool
getNameType (Str _) = TStr
getNameType (Void _) = TVoid

getNameArg :: Arg -> TypeName
getNameArg (ArgVal _ type_ _) = getNameType type_
getNameArg (ArgRef _ type_ _) = getNameType type_

data TypeCheckException =
    NoMainException |
    ReturnOutsideFunctionException BNFC'Position |
    MultipleDefinitionException String BNFC'Position BNFC'Position |
    TypeMismatchException TypeName TypeName BNFC'Position |
    InvalidFunctionCallException String BNFC'Position [TypeName] [TypeName] |
    ParameterDuplicatedException String BNFC'Position BNFC'Position |
    VariableNotFoundException String BNFC'Position |
    NoReturnException String BNFC'Position |
    ReturnMismatchException TypeName TypeName BNFC'Position |
    NoIdentPassedAsRefException String BNFC'Position |
    FunctionNotFoundException String BNFC'Position |
    VoidTypeVariableException BNFC'Position

positionToString :: BNFC'Position -> String
positionToString (Just (line, column)) = "(line: " ++ show line ++ ", column: " ++ show column ++ ")"
positionToString Nothing = ""

instance Show TypeCheckException where
    show NoMainException = "No main function"
    show (ReturnOutsideFunctionException pos) = "Return statement outside function at " 
        ++ positionToString pos
    show (MultipleDefinitionException name pos1 pos2) =
        "Variable " ++ name ++" is defined multiple times in the same scope at "
        ++ positionToString pos1 ++ " and " ++ positionToString pos2
    show (TypeMismatchException expected actual pos) =
        "Type mismatch: expected " ++ show expected ++ ", got " ++ show actual 
        ++ " at " ++ positionToString pos
    show (ParameterDuplicatedException name pos1 pos2) =
        "Parameter " ++ name ++" is duplicated at "
        ++ positionToString pos1 ++ " and " ++ positionToString pos2
    show (VariableNotFoundException name pos) = "Variable " ++ name 
        ++ " not found at " ++ positionToString pos
    show (NoReturnException name pos) = "No return statement in function " 
        ++ name ++ " defined at " ++ positionToString pos
    show (ReturnMismatchException expected actual pos) =
        "Return type mismatch: expected: " ++ show expected ++ ", got: " 
        ++ show actual ++ " at " ++ positionToString pos
    show (NoIdentPassedAsRefException name pos) = "No identifier passed to function " 
        ++ name ++ " at " ++ positionToString pos
    show (FunctionNotFoundException name pos) = "Function " ++ name 
        ++ " not found at " ++ positionToString pos
    show (InvalidFunctionCallException name pos expected actual) = 
        "Invalid function " ++ name ++ " call: expected arguments: " 
        ++ show expected ++ ", got: " ++ show actual ++ " at " 
        ++ positionToString pos
    show (VoidTypeVariableException pos) = "Variable cannot have void type at " 
        ++ positionToString pos

parseMultipleDefinition :: [Item] -> [Ident]
parseMultipleDefinition [] = []
parseMultipleDefinition ((Init _ name _):xs) = name : parseMultipleDefinition xs
parseMultipleDefinition ((NoInit _ name):xs) = name : parseMultipleDefinition xs

getInitNamesTopDef :: TopDef -> [Ident]
getInitNamesTopDef (FnDef _ _ (Ident name) _ _) = [Ident name]
getInitNamesTopDef (VarDef _ _ inits_) = do
    parseMultipleDefinition inits_

getInitNamesStmt :: Stmt -> [Ident]
getInitNamesStmt (FnDecl _ _ (Ident name) _ _) = [Ident name]
getInitNamesStmt (VarDecl _ _ inits_) = do
    parseMultipleDefinition inits_
getInitNamesStmt _ = []

getInitNamesItem :: Item -> [Ident]
getInitNamesItem (Init _ name _) = [name]
getInitNamesItem (NoInit _ name) = [name]

getInitNamesArg :: Arg -> [Ident]
getInitNamesArg (ArgVal _ _ name) = [name]
getInitNamesArg (ArgRef _ _ name) = [name]

getTypeNamesArg :: Arg -> [TypeName]
getTypeNamesArg (ArgVal _ type_ _) = [getNameType type_]
getTypeNamesArg (ArgRef _ type_ _) = [getNameType type_]

getExpressionFromItem :: Item -> Maybe Expr
getExpressionFromItem (Init _ _ expr) = Just expr
getExpressionFromItem (NoInit _ _) = Nothing

getNamesDefinedMultipleTimes :: [Ident] -> Maybe Ident
getNamesDefinedMultipleTimes names = do
    let duplicates = names \\ nub names
    if not (null duplicates)
        then Just (head duplicates)
        else Nothing

getPositionsOfDuplicatesItem :: String -> Item -> [BNFC'Position]
getPositionsOfDuplicatesItem name (Init pos (Ident name') _) = [pos | name == name']
getPositionsOfDuplicatesItem name (NoInit pos (Ident name')) = [pos | name == name']

getPositionsOfDuplicatesTD :: Ident -> TopDef -> [BNFC'Position]
getPositionsOfDuplicatesTD (Ident name') (FnDef pos _ (Ident name) _ _) = 
    [pos | name == name']
getPositionsOfDuplicatesTD (Ident name) (VarDef _ _ inits_) = 
    concatMap (getPositionsOfDuplicatesItem name) inits_

getPositionsOfDuplicatesStmt :: Ident -> Stmt -> [BNFC'Position]
getPositionsOfDuplicatesStmt (Ident name') (FnDecl pos _ (Ident name) _ _) = 
    [pos | name == name']
getPositionsOfDuplicatesStmt (Ident name) (VarDecl _ _ inits_) = 
    concatMap (getPositionsOfDuplicatesItem name) inits_
getPositionsOfDuplicatesStmt _ _ = []

getPositionsOfDuplicatesInit :: Ident -> Item -> [BNFC'Position]
getPositionsOfDuplicatesInit (Ident name') (Init pos (Ident name) _) = 
    [pos | name == name']
getPositionsOfDuplicatesInit (Ident name) (NoInit pos (Ident name')) = 
    [pos | name == name']

getPositionsOfDuplicatesArg :: Ident -> Arg -> [BNFC'Position]
getPositionsOfDuplicatesArg (Ident name) (ArgVal pos _ (Ident name')) = 
    [pos | name == name']
getPositionsOfDuplicatesArg (Ident name) (ArgRef pos _ (Ident name')) = 
    [pos | name == name']

getPositionFromExpr :: Expr -> BNFC'Position
getPositionFromExpr (EVar position _) = position
getPositionFromExpr (ELitInt position _) = position
getPositionFromExpr (ELitTrue position) = position
getPositionFromExpr (ELitFalse position) = position
getPositionFromExpr (EString position _) = position
getPositionFromExpr (Neg position _) = position
getPositionFromExpr (Not position _) = position
getPositionFromExpr (EApp position _ _) = position
getPositionFromExpr (EMul position _ _ _) = position
getPositionFromExpr (EAdd position _ _ _) = position
getPositionFromExpr (ERel position _ _ _) = position
getPositionFromExpr (EAnd position _ _) = position
getPositionFromExpr (EOr position _ _) = position

areRefsIdents :: [Arg] -> [Expr] -> Bool
areRefsIdents [] [] = True
areRefsIdents ((ArgVal _ _ (Ident _)):xs) (_:ys) = areRefsIdents xs ys
areRefsIdents ((ArgRef _ _ (Ident _)):xs) ((EVar _ (Ident _)):ys) = 
    areRefsIdents xs ys
areRefsIdents _ _ = False