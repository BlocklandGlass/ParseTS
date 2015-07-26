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

type Block = [WithSourcePos Statement]

data WithSourcePos a = WithSourcePos SourcePos a
                   deriving (Eq, Show)

data TopLevel = TopLevelDef (WithSourcePos Definition)
              | TopLevelStatement (WithSourcePos Statement)
              deriving (Eq, Show)

data Definition = FunctionDef Function
                | PackageDef PackageName [Function]
                deriving (Eq, Show)

data Function = Function FunctionName [VariableName] Block
              deriving (Eq, Show)

data Statement = ExprStatement Expression
               | IfStatement (WithSourcePos Expression) Block Block
               | ReturnStatement (WithSourcePos Expression)
               deriving (Eq, Show)

data Expression = StrLiteralExpression String
                | NumberLiteralExpression String
                | BoolLiteralExpression Bool
                | LocalVarExpression VariableName
                | GlobalVarExpression VariableName
                | FunctionCallExpression FunctionName [Expression]

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
