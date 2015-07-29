-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  teo@nullable.se
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
main
) where

import Language.TorqueScript

import System.Exit

main :: IO ()
main = do
    analysisResult <- analyzeFromFile "test.cs"
    print analysisResult
    either (const exitFailure) (const exitSuccess) analysisResult
