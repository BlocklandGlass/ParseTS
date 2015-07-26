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

import qualified Data.Text.IO as TIO
import Text.Parsec.Error
import Language.TorqueScript.AST
import Language.TorqueScript.Tokens
import Language.TorqueScript.Tokenizer
import Language.TorqueScript.Parser

parseFromFile :: FilePath -> IO (Either ParseError [TopLevel])
parseFromFile path = do
    contents <- TIO.readFile path
    return $ parseTS path contents

main :: IO ()
main = parseFromFile "test.cs" >>= print
