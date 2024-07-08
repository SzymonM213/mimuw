module Evaluator.Utils where
import Parser.Abs

data EvaluateException = 
    DivByZeroException BNFC'Position |
    InvalidReferenceException String BNFC'Position |
    NoReturnException String BNFC'Position |
    UnexpectedException -- Exception that should not happen after checking types

instance Show EvaluateException where
    show (DivByZeroException pos) = "Division by zero at " ++ positionToString pos
    show (InvalidReferenceException name pos) = "Invalid reference when calling " 
        ++ name ++ " at " ++ positionToString pos
    show (NoReturnException name pos) = "No return statement when leaving " 
        ++ name ++ " at " ++ positionToString pos
    show UnexpectedException = "Unexpected exception"

positionToString :: BNFC'Position -> String
positionToString (Just (line, column)) = "(line: " ++ show line ++ ", column: " 
    ++ show column ++ ")"
positionToString Nothing = "(unknown position)"

isPrint :: Ident -> Bool
isPrint (Ident "printInt") = True
isPrint (Ident "printBool") = True
isPrint (Ident "printString") = True
isPrint _ = False

isVoid :: Type -> Bool
isVoid (Void _) = True
isVoid _ = False