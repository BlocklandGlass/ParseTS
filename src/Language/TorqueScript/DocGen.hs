module Language.TorqueScript.DocGen where

import Language.TorqueScript.AST
import Language.TorqueScript.Analysis

import Text.Hamlet

docFunctionId :: Function -> String
docFunctionId (Function _ name _ _) = "function-" ++ name

docFunction :: WithSourcePos Function -> Html
docFunction (WithSourcePos _ function@(Function docstring name args _)) = [shamlet|
<div class="function" id=#{docFunctionId function}>
    <h3> #{name}
    $if not $ null args
        <div>
            <h4> Arguments
            <ul>
                $forall arg <- args
                    <li> %#{arg}
    <div>
        $forall docstringLine <- docstring
            <p> #{docstringLine}
|]

docFunctions :: [WithSourcePos Function] -> Html
docFunctions functions = [shamlet|
<div>
    <h2> Functions
    <ul class="functions">
        $forall f <- functions
            <li> #{docFunction f}
|]

docPackage :: WithSourcePos Package -> Html
docPackage (WithSourcePos _ (Package name functions)) = [shamlet|
<div>
    <h3> #{name}
    #{docFunctions functions}
|]

docPackages :: [WithSourcePos Package] -> Html
docPackages packages = [shamlet|
<div>
    <h2> Packages
    <ul>
        $forall package <- packages
            <li> #{docPackage package}
|]

docFile :: String -> [TopLevel] -> Html
docFile name file = [shamlet|
<h1> #{name}
#{docFunctions $ allFunctions False file}
#{docPackages $ allPackages file}
|]

