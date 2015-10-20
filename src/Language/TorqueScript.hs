module Language.TorqueScript(tokenizeFromFile, parseFromFile, analyzeFromFile, AnalysisResult(..)) where

import Language.TorqueScript.AST
import Language.TorqueScript.Tokens(Token)
import Language.TorqueScript.Tokenizer(tokenize)
import Language.TorqueScript.Parser
import Language.TorqueScript.Rules

import Control.Applicative
import qualified Data.Text as T
import qualified  Data.Text.IO as TIO
import Text.Parsec.Pos(SourcePos)
import Text.Parsec.Error(ParseError, errorPos, errorMessages, messageString, showErrorMessages)

tokenizeFromFile :: FilePath -> IO (Either ParseError [(SourcePos, Token)])
tokenizeFromFile path = do
    contents <- TIO.readFile path
    let fixedContent = T.replace "\t" "    " contents -- Contrary to everyone else, Haskell treats a tab as 8 spaces, rather than 4
    return $ tokenize path fixedContent

parseFromFile :: FilePath -> IO (Either ParseError [TopLevel])
parseFromFile path = (>>= parseTokens path) <$> tokenizeFromFile path

parseErrorAnalysisResult :: ParseError -> AnalysisResult
parseErrorAnalysisResult err = AnalysisResult
                               { analysisComplaints = [ WithSourcePos (errorPos err)
                                                      $ ParserError
                                                      $ showErrorMessages "or" "unknown parse error"
                                                                          "expecting" "unexpected" "end of input"
                                                      $ errorMessages err
                                                      ]
                               , analysisFunctions = []
                               , analysisPackages = []
                               }

analyzeFromFile :: FilePath -> IO AnalysisResult
analyzeFromFile path = either parseErrorAnalysisResult id . fmap analyzeAST <$> parseFromFile path
