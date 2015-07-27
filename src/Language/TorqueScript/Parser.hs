-----------------------------------------------------------------------------
--
-- Module      :  Language.TorqueScript.Parser
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

module Language.TorqueScript.Parser (parseTokens, parseTS) where

import Language.TorqueScript.AST
import Language.TorqueScript.Tokens
import Language.TorqueScript.Tokenizer(tokenize)

import Text.Parsec.Prim
import Text.Parsec.Combinator hiding(anyToken)
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Expr

import Control.Applicative hiding((<|>), many)
import Control.Monad.Identity(Identity)
import Data.Maybe(fromMaybe)
import Data.Text(Text)

type TSState = ()
type Parser a = Parsec [(SourcePos, Token)] TSState a

satisfyMatch :: (Token -> Maybe a) -> Parser a
satisfyMatch test = tokenPrim show nextPos testToken
    where nextPos _ (x, _) _ = x
          testToken (_, x) = test x

satisfy :: (Token -> Bool) -> Parser Token
satisfy test = satisfyMatch testToken
    where testToken x = if test x then Just x else Nothing

staticToken :: Token -> Parser Token
staticToken x = satisfy (== x) <?> show x

nameToken :: Parser String
nameToken = satisfyMatch getNameToken <?> "NameToken"

functionNameToken :: Parser String
functionNameToken = (\a b -> a ++ "::" ++ b) <$> nameToken <* staticToken DoubleColonToken <*> nameToken

localVarToken :: Parser String
localVarToken = satisfyMatch getLocalVarToken <?> "LocalVarToken"

parens :: Parser a -> Parser a
parens = between (staticToken ParenBeginToken) (staticToken ParenEndToken)

brackets :: Parser a -> Parser a
brackets = between (staticToken BracketBeginToken) (staticToken BracketEndToken)

withSourcePos :: Parser a -> Parser (WithSourcePos a)
withSourcePos parser = do
    pos <- getPosition
    value <- parser
    return $ WithSourcePos pos value

comma :: Parser Token
comma = staticToken CommaToken

literal :: Parser Expression
literal = satisfyMatch test <?> "literal"
    where test (StrToken x) = Just $ StrLiteralExpression x
          test (NumToken x) = Just $ NumberLiteralExpression x
          test TrueKeyword = Just $ BoolLiteralExpression True
          test FalseKeyword = Just $ BoolLiteralExpression False
          test _ = Nothing

variableRef :: Parser Expression
variableRef = satisfyMatch test <?> "variable"
        where test (LocalVarToken x) = Just $ LocalVarExpression x
              test (GlobalVarToken x) = Just $ GlobalVarExpression x
              test _ = Nothing

nameRef :: Parser Expression
nameRef = NameLiteralExpression <$> nameToken

functionCall :: Parser Expression
functionCall = FunctionCallExpression
           <$> try (functionNameToken <* staticToken ParenBeginToken) -- Not using parens since we commit after the opening paren
           <*> expr `sepBy` comma
           <*  staticToken ParenEndToken

objectBody :: Parser [ObjectMember]
objectBody = brackets
           $ many
           $ ObjectMember
         <$> nameToken
         <*> (staticToken AssignToken *> expr)
         <*  semicolon

newObject :: Parser Expression
newObject = staticToken NewKeyword
         *> (NewObjectExpression
        <$> nameToken
        <*> parens (optionMaybe expr)
        <*> maybeBody)
    where maybeBody = fromMaybe [] <$> optionMaybe objectBody

term :: Parser Expression
term = choice
     [ functionCall
     , literal
     , variableRef
     , newObject
     , nameRef
     , parens expr
     ]

opTable :: OperatorTable [(SourcePos, Token)] TSState Identity Expression
opTable = [ []
          , [binLeft MultiplyToken NumberMultiplyExpression, binLeft DivideToken NumberDivideExpression, binLeft ModuloToken NumberModuleExpression]
          , [binLeft AddToken NumberAddExpression, binLeft SubtractToken NumberSubtractExpression]
          , [binLeft AppendToken StringAppendExpression, binLeft SpcKeyword (strBetween " "), binLeft TabKeyword (strBetween "\t")]
          , [binLeft NumEqualsToken NumberEqualsExpression, binLeft NumNoEqualsToken NumberNoEqualsExpression, binLeft StrEqualsToken StringEqualsExpression, binLeft StrNoEqualsToken StringNoEqualsExpression]
          ]
    where binary token func = Infix (func <$ staticToken token)
          binLeft token func = binary token func AssocLeft
          strBetween between a b = a `StringAppendExpression` StrLiteralExpression between `StringAppendExpression` b

expr :: Parser Expression
expr = buildExpressionParser opTable term

semicolon :: Parser ()
semicolon = () <$ staticToken SemicolonToken

ifStatement :: Parser Statement
ifStatement = staticToken IfKeyword
           *> (IfStatement
          <$> parens (withSourcePos expr)
          <*> blockOrStatement
          <*> maybeElse)
    where elseStatement = staticToken ElseKeyword *> blockOrStatement
          maybeElse = fromMaybe [] <$> optionMaybe elseStatement

returnStatement :: Parser Statement
returnStatement = staticToken ReturnKeyword
               *> (ReturnStatement
              <$> optionMaybe (withSourcePos expr))
              <*  semicolon

statement :: Parser Statement
statement = choice
          [ ExprStatement <$> expr <* semicolon
          , ifStatement
          , returnStatement
          ]

block :: Parser Block
block = brackets $ many $ withSourcePos statement

blockOrStatement :: Parser Block
blockOrStatement = block <|> ((: []) <$> withSourcePos statement)

functionDef :: Parser Definition
functionDef = FunctionDef <$> function
    where function = staticToken FunctionKeyword
                  *> (Function
                 <$> functionNameToken
                 <*> argList
                 <*> block)
          arguments = localVarToken `sepBy` comma
          argList = parens arguments

topLevel :: Parser TopLevel
topLevel = choice
    [ TopLevelDef <$> withSourcePos functionDef
    , TopLevelStatement <$> withSourcePos statement
    ]

parseTokens :: SourceName -> [(SourcePos, Token)] -> Either ParseError [TopLevel]
parseTokens = parse (many topLevel <* eof)

parseTS :: SourceName -> Text -> Either ParseError [TopLevel]
parseTS name input = tokenize name input >>= parseTokens name
