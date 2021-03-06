module Language.TorqueScript.Tokenizer (tokenize) where

import Language.TorqueScript.Tokens
import Text.Parsec
import Text.Parsec.Text
import Data.Maybe
import Data.Text(Text)

withSourcePos :: Parser a -> Parser (SourcePos, a)
withSourcePos parser = (,) <$> getPosition <*> parser

anyExcept :: String -> Parser Char
anyExcept characters = satisfy (`notElem` characters)

stringChar :: Char -> Parser String
stringChar surrounding = choice $ try <$>
                       [ string ['\\', surrounding]
                       , string "\\\\"
                       , (: []) <$> anyExcept [surrounding, '\r', '\n']
                       ]

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
    , WhileKeyword <$ string "while"
    , DoKeyword <$ string "do"
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
    where namespaceSuffix = (++) <$> many1 (char ':') <*> many1 nameChar
          namespaceSuffixes = concat <$> many (try namespaceSuffix)

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
assign = try (AssignToken <$ char '=' <* notFollowedBy (char '='))

questionMark :: Parser Token
questionMark = QuestionMarkToken <$ char '?'

doubleColon :: Parser Token
doubleColon = DoubleColonToken <$ try (string "::")

singleColon :: Parser Token
singleColon = SingleColonToken <$ char ':'

dot :: Parser Token
dot = DotToken <$ char '.'

plainComment :: Parser (Maybe a)
plainComment = Nothing <$ (try (string "//") *> manyTill anyChar (char '\n'))

docComment :: Parser (Maybe (SourcePos, Token))
docComment = Just <$> withSourcePos (DocCommentToken <$> (try (string "//|") *> manyTill anyChar (char '\n')))

comment :: Parser (Maybe (SourcePos, Token))
comment = docComment <|> plainComment

whitespace :: Parser (Maybe a)
whitespace = Nothing <$ many1 (char ' ' <|> char '\t' <|> char '\n' <|> char '\r')

comparison :: Parser Token
comparison = choice $ try <$>
    [ NumEqualsToken <$ string "=="
    , NumNoEqualsToken <$ string "!="
    , LessThanToken <$ char '<' <* notFollowedBy (char '=')
    , LessThanOrEqualsToken <$ string "<="
    , GreaterThanToken <$ char '>' <* notFollowedBy (char '=')
    , GreaterThanOrEqualsToken <$ string ">="
    , StrEqualsToken <$ string "$="
    , StrNoEqualsToken <$ string "!$="
    , OrToken <$ string "||"
    , AndToken <$ string "&&"
    ]

strOps :: Parser Token
strOps = choice
    [ AppendToken <$ char '@'
    ]

numOps :: Parser Token
numOps = choice
    [ IncrementToken <$ try (string "++")
    , DecrementToken <$ try (string "--")
    , AssignAddToken <$ try (string "+=")
    , AssignSubtractToken <$ try (string "-=")
    , AssignMultiplyToken <$ try (string "*=")
    , AssignDivideToken <$ try (string "/=")
    , AssignModuloToken <$ try (string "%=")
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
    , questionMark
    , doubleColon
    , singleColon
    , dot
    , strOps
    , numOps
    , boolOps
    , name
    ]
tsTokens :: Parser [(SourcePos, Token)]
tsTokens = catMaybes <$> many (comment <|> whitespace <|> Just <$> tsToken) <* eof

tokenize :: SourceName -> Text -> Either ParseError [(SourcePos, Token)]
tokenize = parse tsTokens
