module Main(main) where

import qualified Docs
import qualified Lint
import qualified DumpTokens

import Control.Applicative
import System.Directory
import System.Environment
import System.FilePath((</>), takeExtension)

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
main = do
    args <- getArgs
    case args of
        ["lint", path] -> fileTree path "" >>= sequence_ . fmap Lint.mainFile
        ["docs", path] -> fileTree path "" >>= sequence_ . fmap Docs.mainFile
        ["dump-tokens", path] -> fileTree path "" >>= sequence_ . fmap DumpTokens.mainFile
        _ -> error "Usage: parsets lint|docs|dump-tokens <path>"
