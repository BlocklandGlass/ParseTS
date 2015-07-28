-----------------------------------------------------------------------------
--
-- Module      :  Language.TorqueScript.AST
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

data ObjectMember = ObjectMember VariableName Expression
                  deriving (Eq, Show)

data TopLevel = TopLevelDef (WithSourcePos Definition)
              | TopLevelStatement (WithSourcePos Statement)
              deriving (Eq, Show)

data Definition = FunctionDef Function
                | PackageDef { pkgDefName :: PackageName, pkgDefBody :: [Function] }
                deriving (Eq, Show)

data Function = Function { funcDefName :: FunctionName, funcDefParams :: [VariableName], funcDefBody :: Block }
              deriving (Eq, Show)

data Reference = LocalVarReference VariableName
               | GlobalVarReference VariableName
               | FieldReference Expression VariableName
               deriving (Eq, Show)

data SwitchCase = SwitchCase Expression Block
                | SwitchDefaultCase Block
                deriving (Eq, Show)
type SwitchCases = [WithSourcePos SwitchCase]

data Statement = ExprStatement Expression
               | IfStatement { ifStmtCond :: WithSourcePos Expression, ifStmtTrue :: Block, ifStmtFalse :: Block }
               | NumSwitchStatement { numSwitchStmtCond :: WithSourcePos Expression, numSwitchStmtBody :: SwitchCases }
               | StrSwitchStatement { strSwitchStmtCond :: WithSourcePos Expression, strSwitchStmtBody :: SwitchCases }
               | ForStatement { forStmtSetup :: Maybe (WithSourcePos Expression), forStmtCond :: WithSourcePos Expression, forStmtBetween :: Maybe (WithSourcePos Expression), forStmtBody :: Block }
               | ReturnStatement (Maybe (WithSourcePos Expression))
               deriving (Eq, Show)

data Expression = StrLiteralExpression String
                | NameLiteralExpression String
                | TaggedStrLiteralExpression String
                | NumberLiteralExpression String
                | BoolLiteralExpression Bool
                | ReferenceExpression Reference
                | AssignExpression Reference Expression
                | FunctionCallExpression FunctionName [Expression]
                | NewObjectExpression ObjectBase (Maybe Expression) [ObjectMember]
                | MethodCallExpression Expression FieldName [Expression]

                -- Comparisons
                | NumberEqualsExpression Expression Expression
                | NumberNoEqualsExpression Expression Expression
                | NumberLessThanExpression Expression Expression
                | NumberGreaterThanExpression Expression Expression
                | NumberLessThanOrEqualsExpression Expression Expression
                | NumberGreaterThanOrEqualsExpression Expression Expression
                | BoolOrExpression Expression Expression
                | BoolAndExpression Expression Expression
                | StringEqualsExpression Expression Expression
                | StringNoEqualsExpression Expression Expression

                -- String operations
                | StringAppendExpression Expression Expression

                -- Number operations
                | NumberAddExpression Expression Expression
                | NumberSubtractExpression Expression Expression
                | NumberMultiplyExpression Expression Expression
                | NumberDivideExpression Expression Expression
                | NumberModuleExpression Expression Expression
                deriving (Eq, Show)
