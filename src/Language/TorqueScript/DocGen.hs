module Language.TorqueScript.DocGen where

import Language.TorqueScript.AST
import Language.TorqueScript.Analysis

import Control.Monad
import Text.Blaze.Html5((!), toValue, toMarkup, Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data DocNode = DocDirectory FilePath [DocNode]
             | DocFile FilePath [DocNode]

docFunctionId :: Function -> String
docFunctionId (Function _ name _ _) = "function-" ++ name

docFunction :: WithSourcePos Function -> Html
docFunction (WithSourcePos _ function@(Function docstring name args _)) =
  H.div ! A.class_ "function" ! A.id (toValue $ docFunctionId function) $ do
    H.h3 $ toMarkup name
    unless (null args) $
      H.div $ do
        H.h4 "Arguments"
        H.ul $
          sequence_ $ H.li . toMarkup . ('%' :) <$> args
    H.div $
      sequence_ $ toMarkup <$> docstring

docFunctions :: [WithSourcePos Function] -> Html
docFunctions functions =
  H.div $ do
    H.h2 "Functions"
    H.ul ! A.class_ "functions" $
      sequence_ $ docFunction <$> functions

docPackage :: WithSourcePos Package -> Html
docPackage (WithSourcePos _ (Package name functions)) =
  H.div $ do
    H.h3 $ toMarkup name
    docFunctions functions

docPackages :: [WithSourcePos Package] -> Html
docPackages packages =
  H.div $ do
    H.h2 "Packages"
    H.ul $
      sequence_ $ docPackage <$> packages

docFile :: String -> [TopLevel] -> Html
docFile name file =
  H.html $ do
    H.head $
      H.title $ do
        _ <- "ParseTS - "
        toMarkup name
    H.body $ do
      H.h1 $ toMarkup name
      docFunctions $ allFunctions False file
      docPackages $ allPackages file
