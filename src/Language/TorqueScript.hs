module Language.TorqueScript(analyze, analyzeFromFile) where

import Language.TorqueScript.Parser
import Language.TorqueScript.Rules

import Control.Applicative
import Data.Text(Text)
import qualified  Data.Text.IO as TIO
import Text.Parsec.Error(ParseError)
import Text.Parsec.Pos(SourceName)

analyze :: SourceName -> Text -> Either ParseError AnalysisResult
analyze name contents = analyzeAST <$> parseTS name contents

analyzeFromFile :: FilePath -> IO (Either ParseError AnalysisResult)
analyzeFromFile path = do
    contents <- TIO.readFile path
    return $ analyze path contents
