{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lint where

import Language.TorqueScript
import Language.TorqueScript.AST(WithSourcePos(..))

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC8
import qualified Data.HashMap.Strict as M
import System.Exit
import System.IO
import Text.Parsec.Pos

instance ToJSON AnalysisResult where
    toJSON result = object
                  [ "complaints" .= show (analysisComplaints result)
                  , "functions" .= analysisFunctions result
                  , "packages" .= analysisPackages result
                  ]

instance ToJSON a => ToJSON (WithSourcePos a) where
    toJSON (WithSourcePos pos x) = combine (toJSON x) (toJSON pos)
        where combine (Object xJSON) (Object posJSON) = Object $ M.union xJSON posJSON
              combine xJSON posJSON@(Object _) = combine (object
                                               [ "value" .= xJSON
                                               ]) posJSON
              combine _ _ = error "toJSON pos wasn't an object, this should never happen!"

instance ToJSON SourcePos where
    toJSON pos = object
               [ "line" .= sourceLine pos
               , "column" .= sourceColumn pos
               , "file" .= sourceName pos
               ]

mainFile :: FilePath -> IO ()
mainFile path = do
    hPutStrLn stderr $ "Parsing " ++ path
    analysisResult <- analyzeFromFile path
    either (\a -> print a >> exitFailure) (BSC8.putStrLn . encode) analysisResult
