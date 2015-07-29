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

anyExcept :: Char -> Parser Char
anyExcept char = satisfy (/= char)

stringChar :: Char -> Parser String
stringChar surrounding = try (string ['\\', surrounding]) <|> ((: []) <$> anyExcept surrounding)

anyStringLiteral :: Char -> Parser Token
anyStringLiteral surrounding = StrToken . concat <$> (char surrounding *> manyTill (stringChar surrounding) (char surrounding))

stringLiteral :: Parser Token
stringLiteral = anyStringLiteral '"'

taggedStringLiteral :: Parser Token
taggedStringLiteral = anyStringLiteral '\''

int :: Parser String
int = many1 digit
decimals :: Parser String
decimals = (:) <$> char '.' <*> int
number :: Parser String
number = (++) <$> int <*> (fromMaybe "" <$> optionMaybe decimals)

numberLiteral :: Parser Token
numberLiteral = NumToken <$> number

literal :: Parser Token
literal = choice
    [ stringLiteral
    , taggedStringLiteral
    , numberLiteral
    ]

keyword :: Parser Token
keyword = choice $ try <$>
    [ FunctionKeyword <$ string "function"
    , PackageKeyword <$ string "package"
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
    , DefaultKeyword <$ string "default"
    , TabKeyword <$ string "TAB"
    , SpcKeyword <$ string "SPC"
    , NlKeyword <$ string "NL"
    , NewKeyword <$ string "new"
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
    , BraceBeginToken <$ char '{'
    , BraceEndToken <$ char '}'
    , BracketBeginToken <$ char '['
    , BracketEndToken <$ char ']'
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
    , LessThanToken <$ char '<' <* notFollowedBy (char '=')
    , LessThanOrEqualsToken <$ string "<="
    , GreaterThanToken <$ char '>' <* notFollowedBy (char '=')
    , GreaterThanOrEqualsToken <$ string ">="
    , StrEqualsToken <$ string "$="
    , StrNoEqualsToken <$ string "!$="
    ]

strOps :: Parser Token
strOps = choice
    [ AppendToken <$ char '@'
    ]

numOps :: Parser Token
numOps = choice
    [ IncrementToken <$ try (string "++")
    , DecrementToken <$ try (string "--")
    , AddToken <$ char '+'
    , SubtractToken <$ char '-'
    , MultiplyToken <$ char '*'
    , DivideToken <$ char '/'
    , ModuloToken <$ char '%'
    ]

boolOps :: Parser Token
boolOps = choice
    [ InvertToken <$ char '!'
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
    , localVarName
    , doubleColon
    , singleColon
    , dot
    , strOps
    , numOps
    , boolOps
    , name
    ]
tsTokens = catMaybes <$> many (comment <|> whitespace <|> Just <$> tsToken) <* eof

tokenize :: SourceName -> Text -> Either ParseError [(SourcePos, Token)]
tokenize = parse tsTokens
