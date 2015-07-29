module Language.TorqueScript.Rules where

import Language.TorqueScript.Analysis
import Language.TorqueScript.AST

import Control.Applicative
import Data.Maybe
import Text.Parsec.Pos(SourcePos)

type Complaint = WithSourcePos String

data AnalysisResult = AnalysisResult
                    { analysisComplaints :: [Complaint]
                    , analysisFunctions :: [WithSourcePos FunctionName]
                    , analysisPackages :: [WithSourcePos PackageName]
                    }
                    deriving (Eq, Show)

evilFunctions :: [String]
evilFunctions = [ "eval"
                , "call"
                , "schedule"
                ]

complainAboutEvilFunctions :: HasSubExprs a => a -> [Complaint]
complainAboutEvilFunctions tree = catMaybes $ checkEvilCall . stripObjectInfo <$> walkFunctionCalls tree
    where checkEvilCall (WithSourcePos pos (FunctionCall "schedule" (_ : WithSourcePos _ (StrLiteralExpression name) : args))) = checkEvilCall $ WithSourcePos pos $ FunctionCall name args
          checkEvilCall (WithSourcePos pos (FunctionCall name _)) | name `elem` evilFunctions = Just $ WithSourcePos pos $ "Call to evil function \"" ++ name ++ "\"! D:"
                                                                  | otherwise = Nothing
          checkEvilCall (WithSourcePos pos _) = Just $ WithSourcePos pos "This shouldn't happen!"
          stripObjectInfo (WithSourcePos pos call@(FunctionCall _ _)) = WithSourcePos pos call
          stripObjectInfo (WithSourcePos pos (MethodCall _ name args)) = WithSourcePos pos (FunctionCall name args)

analyzeAST :: [TopLevel] -> AnalysisResult
analyzeAST tree = AnalysisResult (complainAboutEvilFunctions tree) (fmap funcDefName <$> allFunctions tree) (fmap pkgDefName <$> allPackages tree)
