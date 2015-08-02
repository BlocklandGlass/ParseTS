module Language.TorqueScript(parseFromFile, analyzeFromFile, AnalysisResult(..)) where

import Language.TorqueScript.AST
import Language.TorqueScript.Parser
import Language.TorqueScript.Rules

import Control.Applicative
import qualified Data.Text as T
import qualified  Data.Text.IO as TIO
import Text.Parsec.Error(ParseError)

parseFromFile :: FilePath -> IO (Either ParseError [TopLevel])
parseFromFile path = do
    contents <- TIO.readFile path
    let fixedContent = T.replace "\t" "    " contents -- Contrary to everyone else, Haskell treats a tab as 8 spaces, rather than 4
    return $ parseTS path fixedContent

analyzeFromFile :: FilePath -> IO (Either ParseError AnalysisResult)
analyzeFromFile path = fmap analyzeAST <$> parseFromFile path
