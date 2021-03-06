module Main(main) where

import qualified Docs
import qualified Lint
import qualified DumpTokens

import Language.TorqueScript.Rules(ComplaintSeverity(..))

import System.Directory
import System.Environment
import System.FilePath((</>), takeExtension)
import System.Exit

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

failOnAtLeast :: ComplaintSeverity -> [Maybe ComplaintSeverity] -> IO ()
failOnAtLeast threshold severity | safeMax severity >= Just threshold = exitFailure
                                 | otherwise = exitSuccess
    where safeMax = maximum . (++ [Nothing])

failOnFatal :: [Maybe ComplaintSeverity] -> IO ()
failOnFatal = failOnAtLeast Fatal

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["lint", path] -> fileTree path "" >>= sequence . fmap Lint.mainFile >>= failOnFatal
        ["docs", path] -> fileTree path "" >>= sequence_ . fmap Docs.mainFile
        ["dump-tokens", path] -> fileTree path "" >>= sequence_ . fmap DumpTokens.mainFile
        _ -> error "Usage: parsets lint|docs|dump-tokens <path>"
