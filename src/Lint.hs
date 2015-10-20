{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lint where

import Language.TorqueScript
import Language.TorqueScript.Rules(Complaint, complaintSeverity)
import Language.TorqueScript.AST(WithSourcePos(..))

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC8
import System.IO
import Text.Parsec.Pos

instance ToJSON AnalysisResult where
    toJSON result = object
                  [ "complaints" .= analysisComplaints result
                  , "functions" .= analysisFunctions result
                  , "packages" .= analysisPackages result
                  ]

instance ToJSON a => ToJSON (WithSourcePos a) where
    toJSON (WithSourcePos pos x) = combine (toJSON x) (toJSON pos)
        where combine xJSON posJSON = object
                                    [ "value" .= xJSON
                                    , "pos" .= posJSON
                                    ]

instance ToJSON SourcePos where
    toJSON pos = object
               [ "line" .= sourceLine pos
               , "column" .= sourceColumn pos
               , "file" .= sourceName pos
               ]

instance ToJSON Complaint where
    toJSON complaint = object
                     [ "msg" .= show complaint
                     , "severity" .= show (complaintSeverity complaint) 
                     ]

mainFile :: FilePath -> IO ()
mainFile path = do
    hPutStrLn stderr $ "Parsing " ++ path
    analysisResult <- analyzeFromFile path
    BSC8.putStrLn $ encode analysisResult
