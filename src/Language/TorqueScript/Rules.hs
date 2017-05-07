module Language.TorqueScript.Rules where

import Language.TorqueScript.Analysis
import Language.TorqueScript.AST

data ComplaintSeverity = Info
                       | Warning
                       | Fatal
                       deriving (Eq, Show, Ord)

data Complaint = EvilFunctionCall FunctionName
               | ParserError String
               deriving (Eq)

instance Show Complaint where
    show (EvilFunctionCall name) = "Call to dangerous function: " ++ name
    show (ParserError msg) = "Parser error: " ++ msg

complaintSeverity :: Complaint -> ComplaintSeverity
complaintSeverity (EvilFunctionCall _) = Warning
complaintSeverity (ParserError _) = Fatal

type SPComplaint = WithSourcePos Complaint

data AnalysisResult = AnalysisResult
                    { analysisComplaints :: [SPComplaint]
                    , analysisFunctions :: [WithSourcePos FunctionName]
                    , analysisPackages :: [WithSourcePos PackageName]
                    }
                    deriving (Eq, Show)

type ExprRule = Expression -> [Complaint]

evilFunction :: String -> ExprRule
evilFunction name (CallExpression (FunctionCall calledName _)) | name == calledName = [EvilFunctionCall name]
evilFunction name (CallExpression (MethodCall _ calledName args)) = evilFunction name (CallExpression (FunctionCall calledName args))
evilFunction _ _ = []

dynamicDispatchFunction :: String -> ([SPExpression] -> Maybe Expression) -> ExprRule
dynamicDispatchFunction name tryExpand (CallExpression (MethodCall _ calledName args)) = dynamicDispatchFunction name tryExpand (CallExpression (FunctionCall calledName args))
dynamicDispatchFunction name tryExpand (CallExpression (FunctionCall calledName args)) | name == calledName =
  case tryExpand args of
    Just simplified -> analyzeNodeOnly simplified -- The arguments are already expanded in the proper AST
    Nothing -> [EvilFunctionCall name]
dynamicDispatchFunction _ _ _ = []

simplifyCall :: [SPExpression] -> Maybe Expression
simplifyCall (WithSourcePos _ (StrLiteralExpression name) : args) = Just $ CallExpression $ FunctionCall name args
simplifyCall _ = Nothing

callRule :: ExprRule
callRule = dynamicDispatchFunction "call" simplifyCall

simplifySchedule :: [SPExpression] -> Maybe Expression
simplifySchedule (WithSourcePos _ (NumberLiteralExpression "0") : xs) = simplifyCall xs
simplifySchedule xs = simplifyCall xs

scheduleRule :: ExprRule
scheduleRule = dynamicDispatchFunction "schedule" simplifySchedule

exprRules :: [ExprRule]
exprRules = [ evilFunction "eval"
            , callRule
            , scheduleRule
            ]

analyzeNodeOnly :: Expression -> [Complaint]
analyzeNodeOnly node = concat $ exprRules <*> [node]

analyzeNode :: HasSubExprs a => a -> [SPComplaint]
analyzeNode node = walkSubExprs node >>= flattenWsp . fmap analyzeNodeOnly

analyzeAST :: [TopLevel] -> AnalysisResult
analyzeAST tree = AnalysisResult (analyzeNode tree) (fmap funcDefName <$> allFunctions True tree) (fmap pkgDefName <$> allPackages tree)
