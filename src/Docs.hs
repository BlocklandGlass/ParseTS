module Docs where

import Language.TorqueScript
import Language.TorqueScript.DocGen

import System.Directory
import System.Exit
import System.FilePath((</>), (<.>), takeDirectory)
import System.IO
import Text.Blaze.Html.Renderer.Pretty

outPath :: FilePath -> FilePath
outPath path = "dist" </> "parsets-docs" </> path <.> "html"

mainFile :: FilePath -> IO ()
mainFile path = do
    let out = outPath path
    createDirectoryIfMissing True $ takeDirectory out
    hPutStrLn stderr $ "Rendering " ++ path ++ " to " ++ out
    result <- parseFromFile path
    either (\a -> print a >> exitFailure) (writeFile out . renderHtml . docFile path) result
