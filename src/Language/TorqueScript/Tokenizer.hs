-----------------------------------------------------------------------------
--
-- Module      :  Language.TorqueScript.Tokenizer
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

module Language.TorqueScript.Tokenizer (tokenize) where

import Language.TorqueScript.Tokens
import Text.Parsec
import Text.Parsec.Text
import Data.List
import Data.Maybe
import Data.Text(Text)
import Control.Applicative hiding(many, (<|>))

withSourcePos :: Parser a -> Parser (SourcePos, a)
withSourcePos parser = (,) <$> getPosition <*> parser

stringLiteral :: Parser Token
stringLiteral = StrToken <$> (char '"' *> manyTill anyChar (char '"'))

int :: Parser String
int = many1 digit
decimals :: Parser String
decimals = (:) <$> char '.' <*> int
number :: Parser String
number = (++) <$> int <*> (fromMaybe "" <$> optionMaybe decimals)

numberLiteral :: Parser Token
numberLiteral = NumToken <$> number

literal = choice
    [ stringLiteral
    , numberLiteral
    ]

keyword :: Parser Token
keyword = choice $ try <$>
    [ FunctionKeyword <$ string "function"
    , ReturnKeyword <$ string "return"
    , TrueKeyword <$ string "true"
    , FalseKeyword <$ string "false"
    , IfKeyword <$ string "if"
    , ElseKeyword <$ string "else"
    , ForKeyword <$ string "for"
    , BreakKeyword <$ string "break"
    , ContinueKeyword <$ string "continue"
    , StrSwitchKeyword <$ string "switch$"
    , SwitchKeyword <$ string "switch"
    , CaseKeyword <$ string "case"
    , TabKeyword <$ string "TAB"
    , SpcKeyword <$ string "SPC"
    ]

nameFirstChar :: Parser Char
nameFirstChar = letter <|> char '_'
nameChar :: Parser Char
nameChar = nameFirstChar <|> digit
nameChars :: Parser String
nameChars = (:) <$> nameFirstChar <*> many nameChar
varNameChars :: Parser String
varNameChars = (++) <$> nameChars <*> namespaceSuffixes
    where namespaceSuffix = (++) <$> many1 (char ':') <*> many nameChar
          namespaceSuffixes = concat <$> many namespaceSuffix

name :: Parser Token
name = NameToken <$> nameChars

globalVarName :: Parser Token
globalVarName = GlobalVarToken <$> (char '$' *> varNameChars)

localVarName :: Parser Token
localVarName = LocalVarToken <$> (char '%' *> varNameChars)

parens :: Parser Token
parens = choice
    [ ParenBeginToken <$ char '('
    , ParenEndToken <$ char ')'
    , BracketBeginToken <$ char '{'
    , BracketEndToken <$ char '}'
    ]

semicolon :: Parser Token
semicolon = SemicolonToken <$ char ';'

comma :: Parser Token
comma = CommaToken <$ char ','

assign :: Parser Token
assign = AssignToken <$ char '=' <* notFollowedBy (char '=')

doubleColon :: Parser Token
doubleColon = DoubleColonToken <$ try (string "::")

singleColon :: Parser Token
singleColon = SingleColonToken <$ char ':'

dot :: Parser Token
dot = DotToken <$ char '.'

comment :: Parser (Maybe a)
comment = Nothing <$ (try (string "//") *> manyTill anyChar (char '\n'))

whitespace :: Parser (Maybe a)
whitespace = Nothing <$ many1 (char ' ' <|> char '\t' <|> char '\n' <|> char '\r')

comparison :: Parser Token
comparison = try $ choice
    [ NumEqualsToken <$ string "=="
    , LessThanOrEqualsToken <$ string "<="
    , LessThanToken <$ string "<"
    , GreaterThanOrEqualsToken <$ string ">="
    , GreaterThanToken <$ string ">"
    , StrEqualsToken <$ string "$="
    ]

strOps :: Parser Token
strOps = choice
    [ StrAppendToken <$ char '@'
    ]

numOps :: Parser Token
numOps = choice
    [ NumAddToken <$ char '+'
    , NumSubtractToken <$ char '-'
    , NumMultiplyToken <$ char '*'
    , NumDivideToken <$ char '/'
    , NumModuloToken <$ char '%'
    ]

tsToken :: Parser (SourcePos, Token)
tsToken = choice $ withSourcePos <$>
    [ literal
    , try (keyword <* notFollowedBy nameChar)
    , parens
    , semicolon
    , comma
    , assign
    , comparison
    , globalVarName
    , doubleColon
    , singleColon
    , dot
    , strOps
    , numOps
    , name
    , localVarName
    ]
tsTokens = catMaybes <$> many (comment <|> whitespace <|> Just <$> tsToken) <* eof

tokenize :: SourceName -> Text -> Either ParseError [(SourcePos, Token)]
tokenize = parse tsTokens
