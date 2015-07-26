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
    , SwitchKeyword <$ string "switch"
    , StrSwitchKeyword <$ string "switch$"
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

comment :: Parser (Maybe a)
comment = Nothing <$ (try (string "//") *> manyTill anyChar (char '\n'))

whitespace :: Parser (Maybe a)
whitespace = Nothing <$ many1 (char ' ' <|> char '\t' <|> char '\n' <|> char '\r')

comparison :: Parser Token
comparison = choice
    [ NumEqualsToken <$ string "=="
    , LessThanOrEqualsToken <$ string "<="
    , LessThanToken <$ string "<"
    , GreaterThanOrEqualsToken <$ string ">="
    , GreaterThanToken <$ string ">"
    , StrEqualsToken <$ string "$="
    ]

tsToken :: Parser (SourcePos, Token)
tsToken = choice $ withSourcePos <$>
    [ literal
    , try (keyword <* notFollowedBy nameChar)
    , name
    , localVarName
    , globalVarName
    , parens
    , semicolon
    , comma
    , comparison
    ]
tsTokens = catMaybes <$> many (comment <|> whitespace <|> Just <$> tsToken) <* eof

tokenize :: SourceName -> Text -> Either ParseError [(SourcePos, Token)]
tokenize = parse tsTokens
