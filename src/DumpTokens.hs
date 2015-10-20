{-# OPTIONS_GHC -fno-warn-orphans #-}

module DumpTokens where

import Language.TorqueScript

import System.Exit
import System.IO

mainFile :: FilePath -> IO ()
mainFile path = do
    hPutStrLn stderr $ "Parsing " ++ path
    tokenStream <- tokenizeFromFile path
    either (\a -> print a >> exitFailure) (putStrLn . unlines . fmap show) tokenStream
