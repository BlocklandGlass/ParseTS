module Language.TorqueScript.Analysis where

import Language.TorqueScript.AST

import Data.Maybe(catMaybes)

class HasPackages a where
    allPackages :: a -> [WithSourcePos Package]

instance HasPackages a => HasPackages [a] where
    allPackages xs = xs >>= allPackages

instance HasPackages TopLevel where
    allPackages (TopLevelDef def) = allPackages def
    allPackages _ = []

instance HasPackages (WithSourcePos Definition) where
    allPackages (WithSourcePos pos (PackageDef pkg)) = [WithSourcePos pos pkg]
    allPackages _ = []

class HasFunctions a where
    allFunctions :: a -> [WithSourcePos Function]

instance HasFunctions a => HasFunctions [a] where
    allFunctions xs = xs >>= allFunctions

instance HasFunctions TopLevel where
    allFunctions (TopLevelDef def) = allFunctions def
    allFunctions _ = []

instance HasFunctions (WithSourcePos Definition) where
    allFunctions (WithSourcePos pos (FunctionDef func)) = [WithSourcePos pos func]
    allFunctions _ = []

class HasSubExprs a where
    walkSubExprs :: a -> [SPExpression]

    walkFunctionCalls :: a -> [WithSourcePos Call]
    walkFunctionCalls = catMaybes . fmap filterFunction . walkSubExprs
        where filterFunction (WithSourcePos pos (CallExpression call)) = Just $ WithSourcePos pos call
              filterFunction _ = Nothing

exprAndSubs :: SPExpression -> [SPExpression]
exprAndSubs expr = expr : walkSubExprs expr

exprAndSubsMaybe :: Maybe SPExpression -> [SPExpression]
exprAndSubsMaybe = maybe [] exprAndSubs

instance HasSubExprs a => HasSubExprs (WithSourcePos a) where
    walkSubExprs (WithSourcePos _ a) = walkSubExprs a

instance HasSubExprs a => HasSubExprs [a] where
    walkSubExprs xs = xs >>= walkSubExprs

instance HasSubExprs TopLevel where
    walkSubExprs (TopLevelDef def) = walkSubExprs def
    walkSubExprs (TopLevelStatement stmt) = walkSubExprs stmt

instance HasSubExprs Definition where
    walkSubExprs (PackageDef pkg) = walkSubExprs pkg
    walkSubExprs (FunctionDef func) = walkSubExprs func

instance HasSubExprs Package where
    walkSubExprs (Package _ body) = walkSubExprs body

instance HasSubExprs Function where
    walkSubExprs (Function _ _ body) = walkSubExprs body

instance HasSubExprs Statement where
    walkSubExprs (ExprStatement expr) = exprAndSubs expr
    walkSubExprs (IfStatement cond ifBlock elseBlock) = exprAndSubs cond ++ walkSubExprs ifBlock ++ walkSubExprs elseBlock
    walkSubExprs (NumSwitchStatement expr cases) = exprAndSubs expr ++ walkSubExprs cases
    walkSubExprs (StrSwitchStatement expr cases) = exprAndSubs expr ++ walkSubExprs cases
    walkSubExprs (ForStatement setup cond between body) = exprAndSubsMaybe setup ++ exprAndSubs cond ++ exprAndSubsMaybe between ++ walkSubExprs body
    walkSubExprs (WhileStatement cond body) = exprAndSubs cond ++ walkSubExprs body
    walkSubExprs (DoWhileStatement cond body) = exprAndSubs cond ++ walkSubExprs body
    walkSubExprs (ReturnStatement expr) = exprAndSubsMaybe expr
    walkSubExprs BreakStatement = []
    walkSubExprs ContinueStatement = []

instance HasSubExprs Expression where
    walkSubExprs (StrLiteralExpression _) = []
    walkSubExprs (TaggedStrLiteralExpression _) = []
    walkSubExprs (NumberLiteralExpression _) = []
    walkSubExprs (BoolLiteralExpression _) = []
    walkSubExprs (ReferenceExpression ref) = walkSubExprs ref
    walkSubExprs (AssignExpression ref expr) = walkSubExprs ref ++ exprAndSubs expr
    walkSubExprs (CallExpression call) = walkSubExprs call
    walkSubExprs (NewObjectExpression obj) = walkSubExprs obj
    walkSubExprs (TernaryExpression cond ifExpr elseExpr) = exprAndSubs cond ++ exprAndSubs ifExpr ++ exprAndSubs elseExpr
    walkSubExprs (NumberEqualsExpression a b) = exprAndSubs a ++ exprAndSubs b
    walkSubExprs (NumberNoEqualsExpression a b) = exprAndSubs a ++ exprAndSubs b
    walkSubExprs (NumberLessThanExpression a b) = exprAndSubs a ++ exprAndSubs b
    walkSubExprs (NumberLessThanOrEqualsExpression a b) = exprAndSubs a ++ exprAndSubs b
    walkSubExprs (NumberGreaterThanExpression a b) = exprAndSubs a ++ exprAndSubs b
    walkSubExprs (NumberGreaterThanOrEqualsExpression a b) = exprAndSubs a ++ exprAndSubs b
    walkSubExprs (BoolOrExpression a b) = exprAndSubs a ++ exprAndSubs b
    walkSubExprs (BoolAndExpression a b) = exprAndSubs a ++ exprAndSubs b
    walkSubExprs (StringEqualsExpression a b) = exprAndSubs a ++ exprAndSubs b
    walkSubExprs (StringNoEqualsExpression a b) = exprAndSubs a ++ exprAndSubs b
    walkSubExprs (StringAppendExpression a b) = exprAndSubs a ++ exprAndSubs b
    walkSubExprs (NumberAddExpression a b) = exprAndSubs a ++ exprAndSubs b
    walkSubExprs (NumberSubtractExpression a b) = exprAndSubs a ++ exprAndSubs b
    walkSubExprs (NumberMultiplyExpression a b) = exprAndSubs a ++ exprAndSubs b
    walkSubExprs (NumberDivideExpression a b) = exprAndSubs a ++ exprAndSubs b
    walkSubExprs (NumberModuloExpression a b) = exprAndSubs a ++ exprAndSubs b
    walkSubExprs (BoolInvertExpression a) = walkSubExprs a

instance HasSubExprs SwitchCase where
    walkSubExprs (SwitchCase expr block) = exprAndSubs expr ++ walkSubExprs block
    walkSubExprs (SwitchDefaultCase block) = walkSubExprs block

instance HasSubExprs Reference where
    walkSubExprs (LocalVarReference _) = []
    walkSubExprs (GlobalVarReference _) = []
    walkSubExprs (FieldReference expr _) = exprAndSubs expr
    walkSubExprs (IndexReference ref exprs) = walkSubExprs ref ++ (exprs >>= exprAndSubs)

instance HasSubExprs Call where
    walkSubExprs (FunctionCall _ args) = args >>= exprAndSubs
    walkSubExprs (MethodCall obj _ args) = exprAndSubs obj ++ (args >>= exprAndSubs)

instance HasSubExprs NewObject where
    walkSubExprs (NewObject _ name body) = exprAndSubsMaybe name ++ walkSubExprs body

instance HasSubExprs ObjectMember where
    walkSubExprs (ObjectFieldMember _ expr) = exprAndSubs expr
    walkSubExprs (ObjectIndexedFieldMember _ indexes expr) = (indexes >>= exprAndSubs) ++ exprAndSubs expr
    walkSubExprs (NestedObjectMember obj) = walkSubExprs obj
