module Language.TorqueScript.AST where

import Text.Parsec.Pos(SourcePos)

type PackageName = String
type FunctionName = String
type VariableName = String
type FieldName = String
type ObjectBase = String

type Block = [WithSourcePos Statement]

data WithSourcePos a = WithSourcePos SourcePos a
                   deriving (Eq, Show)
type SPExpression = WithSourcePos Expression

instance Functor WithSourcePos where
    fmap f (WithSourcePos pos a) = WithSourcePos pos $ f a

data ObjectMember = ObjectMember VariableName SPExpression
                  deriving (Eq, Show)

data TopLevel = TopLevelDef (WithSourcePos Definition)
              | TopLevelStatement (WithSourcePos Statement)
              deriving (Eq, Show)

data Function = Function { funcDefName :: FunctionName, funcDefParams :: [VariableName], funcDefBody :: Block }
              deriving (Eq, Show)

data Package = Package { pkgDefName :: PackageName, pkgDefBody :: [WithSourcePos Function] }
             deriving (Eq, Show)

data Definition = FunctionDef Function
                | PackageDef Package
                deriving (Eq, Show)

data Reference = LocalVarReference VariableName
               | GlobalVarReference VariableName
               | FieldReference SPExpression VariableName
               | IndexReference Reference [SPExpression]
               deriving (Eq, Show)

data Call = FunctionCall FunctionName [SPExpression]
          | MethodCall SPExpression FieldName [SPExpression]
          deriving (Eq, Show)

data SwitchCase = SwitchCase SPExpression Block
                | SwitchDefaultCase Block
                deriving (Eq, Show)
type SwitchCases = [WithSourcePos SwitchCase]

data Statement = ExprStatement SPExpression
               | IfStatement { ifStmtCond :: SPExpression, ifStmtTrue :: Block, ifStmtFalse :: Block }
               | NumSwitchStatement { numSwitchStmtCond :: SPExpression, numSwitchStmtBody :: SwitchCases }
               | StrSwitchStatement { strSwitchStmtCond :: SPExpression, strSwitchStmtBody :: SwitchCases }
               | ForStatement { forStmtSetup :: Maybe SPExpression, forStmtCond :: SPExpression, forStmtBetween :: Maybe SPExpression, forStmtBody :: Block }
               | WhileStatement { whileStmtCond :: SPExpression, whileStmtBody :: Block }
               | DoWhileStatement { doWhileStmtCond :: SPExpression, doWhileStmtBody :: Block }
               | ReturnStatement (Maybe SPExpression)
               | BreakStatement
               | ContinueStatement
               deriving (Eq, Show)

data Expression = StrLiteralExpression String
                | TaggedStrLiteralExpression String
                | NumberLiteralExpression String
                | BoolLiteralExpression Bool
                | ReferenceExpression Reference
                | AssignExpression Reference SPExpression
                | CallExpression Call
                | NewObjectExpression ObjectBase (Maybe SPExpression) [ObjectMember]
                | TernaryExpression { ternaryExprCond :: SPExpression, ternaryExprTrue :: SPExpression, ternaryExprFalse :: SPExpression }

                -- Comparisons
                | NumberEqualsExpression SPExpression SPExpression
                | NumberNoEqualsExpression SPExpression SPExpression
                | NumberLessThanExpression SPExpression SPExpression
                | NumberGreaterThanExpression SPExpression SPExpression
                | NumberLessThanOrEqualsExpression SPExpression SPExpression
                | NumberGreaterThanOrEqualsExpression SPExpression SPExpression
                | BoolOrExpression SPExpression SPExpression
                | BoolAndExpression SPExpression SPExpression
                | StringEqualsExpression SPExpression SPExpression
                | StringNoEqualsExpression SPExpression SPExpression

                -- String operations
                | StringAppendExpression SPExpression SPExpression

                -- Number operations
                | NumberAddExpression SPExpression SPExpression
                | NumberSubtractExpression SPExpression SPExpression
                | NumberMultiplyExpression SPExpression SPExpression
                | NumberDivideExpression SPExpression SPExpression
                | NumberModuloExpression SPExpression SPExpression

                -- Bool operations
                | BoolInvertExpression SPExpression
                deriving (Eq, Show)
