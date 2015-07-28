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
import Data.List
import Data.Maybe(fromMaybe, catMaybes)
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
functionNameToken = (\a b -> intercalate "::" $ catMaybes [Just a, b]) <$> nameToken <*> optionMaybe (staticToken DoubleColonToken *> nameToken)

localVarToken :: Parser String
localVarToken = satisfyMatch getLocalVarToken <?> "LocalVarToken"

parens :: Parser a -> Parser a
parens = between (staticToken ParenBeginToken) (staticToken ParenEndToken)

braces :: Parser a -> Parser a
braces = between (staticToken BraceBeginToken) (staticToken BraceEndToken)

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

variableRef :: Parser Reference
variableRef = satisfyMatch test <?> "variable"
        where test (LocalVarToken x) = Just $ LocalVarReference x
              test (GlobalVarToken x) = Just $ GlobalVarReference x
              test _ = Nothing

nameRef :: Parser Expression
nameRef = NameLiteralExpression <$> nameToken

functionCall :: Parser Expression
functionCall = FunctionCallExpression
           <$> try (functionNameToken <* staticToken ParenBeginToken) -- Not using parens since we commit after the opening paren
           <*> expr `sepBy` comma
           <*  staticToken ParenEndToken

objectBody :: Parser [ObjectMember]
objectBody = braces
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
     , ReferenceExpression <$> variableRef
     , newObject
     , nameRef
     , parens expr
     ]

maybeFieldAccess :: Parser Expression
maybeFieldAccess = do
    t <- term
    fieldAccesses <- many fieldAccess
    return $ foldl (flip ($)) t fieldAccesses
    where fieldAccess = staticToken DotToken *> (decideAccessType <$> nameToken <*> optionMaybe (parens $ expr `sepBy` comma))
          decideAccessType :: String -> Maybe [Expression] -> Expression -> Expression
          decideAccessType name (Just args) e = MethodCallExpression e name args
          decideAccessType name Nothing e = ReferenceExpression $ FieldReference e name

maybeAssignment :: Parser Expression
maybeAssignment = do
    e <- maybeFieldAccess
    case e of
        ReferenceExpression ref -> maybe e (AssignExpression ref) <$> optionMaybe (staticToken AssignToken *> expr)
        _ -> return e

opTable :: OperatorTable [(SourcePos, Token)] TSState Identity Expression
opTable = [ []
          , [binLeft MultiplyToken NumberMultiplyExpression, binLeft DivideToken NumberDivideExpression, binLeft ModuloToken NumberModuleExpression]
          , [binLeft AddToken NumberAddExpression, binLeft SubtractToken NumberSubtractExpression]
          , [binLeft AppendToken StringAppendExpression, binLeft SpcKeyword (strBetween " "), binLeft TabKeyword (strBetween "\t")]
          , [binLeft NumEqualsToken NumberEqualsExpression
            ,binLeft NumNoEqualsToken NumberNoEqualsExpression
            ,binLeft LessThanToken NumberLessThanExpression
            ,binLeft LessThanOrEqualsToken NumberLessThanOrEqualsExpression
            ,binLeft GreaterThanToken NumberGreaterThanExpression
            ,binLeft GreaterThanOrEqualsToken NumberGreaterThanOrEqualsExpression
            ,binLeft StrEqualsToken StringEqualsExpression
            ,binLeft StrNoEqualsToken StringNoEqualsExpression
            ]
          ]
    where binary opToken func = Infix (func <$ staticToken opToken)
          binLeft opToken func = binary opToken func AssocLeft
          strBetween between a b = a `StringAppendExpression` StrLiteralExpression between `StringAppendExpression` b

expr :: Parser Expression
expr = buildExpressionParser opTable maybeAssignment

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

switchCase :: Parser SwitchCase
switchCase = staticToken CaseKeyword
          *> (SwitchCase
         <$> expr
         <*  staticToken SingleColonToken
         <*> many (withSourcePos statement))

defaultCase :: Parser SwitchCase
defaultCase = staticToken DefaultKeyword
           *> staticToken SingleColonToken
           *> (SwitchDefaultCase
          <$> many (withSourcePos statement))

switchStatement :: Parser Statement
switchStatement = choice
                [ NumSwitchStatement <$ staticToken SwitchKeyword
                , StrSwitchStatement <$ staticToken StrSwitchKeyword
                ]
              <*> parens (withSourcePos expr)
              <*> braces (many $ choice $ withSourcePos <$> [switchCase, defaultCase])

forStatement :: Parser Statement
forStatement = staticToken ForKeyword
            *> parens (ForStatement
           <$> optionMaybe (withSourcePos expr)
           <*  semicolon
           <*> withSourcePos expr
           <*  semicolon
           <*> optionMaybe (withSourcePos expr))
           <*> blockOrStatement

returnStatement :: Parser Statement
returnStatement = staticToken ReturnKeyword
               *> (ReturnStatement
              <$> optionMaybe (withSourcePos expr))
              <*  semicolon

statement :: Parser Statement
statement = choice
          [ ExprStatement <$> expr <* semicolon
          , ifStatement
          , switchStatement
          , forStatement
          , returnStatement
          ]

block :: Parser Block
block = braces $ many $ withSourcePos statement

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
