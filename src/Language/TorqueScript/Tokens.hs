-----------------------------------------------------------------------------
--
-- Module      :  Language.TorqueScript.Tokens
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

module Language.TorqueScript.Tokens where

import Data.Text

data Token = StrToken String
           | NumToken String
           | NameToken String
           | GlobalVarToken String
           | LocalVarToken String
           -- Keywords
           | FunctionKeyword
           | ReturnKeyword
           | TrueKeyword
           | FalseKeyword
           | IfKeyword
           | ElseKeyword
           | ForKeyword
           | BreakKeyword
           | ContinueKeyword
           | SwitchKeyword
           | StrSwitchKeyword
           | CaseKeyword
           | TabKeyword
           | SpcKeyword
           -- Parens
           | ParenBeginToken
           | ParenEndToken
           | BracketBeginToken
           | BracketEndToken
           | IndexBeginToken
           | IndexEndToken
           -- Punctuation
           | SemicolonToken
           | CommaToken
           | AssignToken
           | DoubleColonToken
           | SingleColonToken
           | DotToken
           -- Comparisons
           | NumEqualsToken
           | NumNoEqualsToken
           | LessThanToken
           | GreaterThanToken
           | LessThanOrEqualsToken
           | GreaterThanOrEqualsToken
           | BoolOrToken
           | BoolAndToken
           | StrEqualsToken
           | StrNoEqualsToken
           -- String operations
           | AppendToken
           -- Number operations
           | AddToken
           | SubtractToken
           | MultiplyToken
           | DivideToken
           | ModuloToken
           -- Bool operations
           | InvertToken
           deriving (Eq, Show)

getStrToken :: Token -> Maybe String
getStrToken (StrToken x) = Just x
getStrToken _ = Nothing

getNumToken :: Token -> Maybe String
getNumToken (NumToken x) = Just x
getNumToken _ = Nothing

getNameToken :: Token -> Maybe String
getNameToken (NameToken x) = Just x
getNameToken _ = Nothing

getGlobalVarToken :: Token -> Maybe String
getGlobalVarToken (GlobalVarToken x) = Just x
getGlobalVarToken _ = Nothing

getLocalVarToken :: Token -> Maybe String
getLocalVarToken (LocalVarToken x) = Just x
getLocalVarToken _ = Nothing