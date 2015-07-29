module Language.TorqueScript.Rules where

import Language.TorqueScript.Analysis
import Language.TorqueScript.AST

import Control.Applicative
import Data.Maybe

data Complaint = EvilFunctionCall FunctionName
               deriving (Eq)

instance Show Complaint where
    show (EvilFunctionCall name) = "Call to evil function \"" ++ name ++ "\"! D:"

type SPComplaint = WithSourcePos Complaint

data AnalysisResult = AnalysisResult
                    { analysisComplaints :: [SPComplaint]
                    , analysisFunctions :: [WithSourcePos FunctionName]
                    , analysisPackages :: [WithSourcePos PackageName]
                    }
                    deriving (Eq, Show)

evilFunctions :: [String]
evilFunctions = [ "eval"
                , "call"
                , "schedule"
                ]

complainAboutEvilFunctions :: HasSubExprs a => a -> [SPComplaint]
complainAboutEvilFunctions tree = catMaybes $ checkEvilCall <$> walkFunctionCalls tree
    where checkEvilCall :: WithSourcePos Call -> Maybe SPComplaint
          checkEvilCall (WithSourcePos pos (FunctionCall "schedule" (_ : WithSourcePos _ (StrLiteralExpression name) : args))) = checkEvilCall $ WithSourcePos pos $ FunctionCall name args
          checkEvilCall (WithSourcePos pos (FunctionCall name _)) | name `elem` evilFunctions = Just $ WithSourcePos pos $ EvilFunctionCall name
                                                                  | otherwise = Nothing
          checkEvilCall (WithSourcePos pos (MethodCall _ name args)) = checkEvilCall $ WithSourcePos pos $ FunctionCall name args

analyzeAST :: [TopLevel] -> AnalysisResult
analyzeAST tree = AnalysisResult (complainAboutEvilFunctions tree) (fmap funcDefName <$> allFunctions tree) (fmap pkgDefName <$> allPackages tree)
