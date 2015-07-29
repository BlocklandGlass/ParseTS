{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Language.TorqueScript
import Language.TorqueScript.AST(WithSourcePos(..))

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC8
import qualified Data.HashMap.Strict as M
import System.Directory
import System.FilePath((</>), takeExtension)
import System.Exit
import Text.Parsec.Pos

instance ToJSON AnalysisResult where
    toJSON result = object
                  [ "complaints" .= analysisComplaints result
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
    putStrLn $ "Parsing " ++ path
    analysisResult <- analyzeFromFile path
    either (\a -> print a >> exitFailure) (BSC8.putStrLn . encode) analysisResult

fileTree :: FilePath -> FilePath -> IO [FilePath]
fileTree _ "." = return []
fileTree _ ".." = return []
fileTree _ ".git" = return []
fileTree base cur = do
    let path = base </> cur
    isDirectory <- doesDirectoryExist path
    if isDirectory then do
        files <- getDirectoryContents path
        subtrees <- sequence $ fileTree path <$> files
        return $ concat subtrees
    else return $ filter isTS [path]
    where isTS filename = takeExtension filename `elem` [".cs", ".gui"]

main :: IO ()
main = fileTree "." "BlocklandGlass" >>= sequence_ . fmap mainFile
