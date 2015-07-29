{-# LANGUAGE CPP #-}

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
import Control.Monad(join)
import Control.Monad.Identity(Identity)
import Data.List
import Data.Maybe(fromMaybe, catMaybes)
import Data.Text(Text)
#ifdef FLAG_DEBUG
import Debug.Trace
#endif

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
variableRef = maybeIndexed <$> variable <*> optionMaybe index
        where test (LocalVarToken x) = Just $ LocalVarReference x
              test (GlobalVarToken x) = Just $ GlobalVarReference x
              test _ = Nothing
              variable = satisfyMatch test <?> "variable"
              index = brackets $ withSourcePos expr `sepBy` comma
              maybeIndexed var (Just indexes) = IndexReference var indexes
              maybeIndexed var Nothing = var

nameRef :: Parser Expression
nameRef = StrLiteralExpression <$> nameToken

functionCall :: Parser Call
functionCall = FunctionCall
           <$> try (functionNameToken <* staticToken ParenBeginToken) -- Not using parens since we commit after the opening paren
           <*> withSourcePos expr `sepBy` comma
           <*  staticToken ParenEndToken

objectBody :: Parser [ObjectMember]
objectBody = braces
           $ many
           $ choice
           [ indexedObjectField
           , objectField
           , NestedObjectMember <$> newObject <* semicolon
           ]

indexedObjectField :: Parser ObjectMember
indexedObjectField = try (ObjectIndexedFieldMember
                 <$> nameToken
                 <*> brackets (many1 $ withSourcePos expr))
                 <*  staticToken AssignToken
                 <*> withSourcePos expr
                 <*  semicolon

objectField :: Parser ObjectMember
objectField = ObjectFieldMember
          <$> nameToken
          <*  staticToken AssignToken
          <*> withSourcePos expr
          <*  semicolon

newObject :: Parser NewObject
newObject = staticToken NewKeyword
         *> (NewObject
        <$> nameToken
        <*> parens (optionMaybe $ withSourcePos expr)
        <*> maybeBody)
    where maybeBody = fromMaybe [] <$> optionMaybe objectBody

term :: Parser Expression
term = choice
     [ CallExpression <$> functionCall
     , literal
     , ReferenceExpression <$> variableRef
     , NewObjectExpression <$> newObject
     , nameRef
     , parens expr
     ]

maybeFieldAccess :: Parser Expression
maybeFieldAccess = do
    pos <- getPosition
    t <- term
    fieldAccesses <- many fieldAccess
    return $ foldl (flip ($)) t ((. WithSourcePos pos) <$> fieldAccesses)
    where fieldAccess = staticToken DotToken *> (flip ($) <$> nameToken <*> choice [indexRef, methodCall, fieldRef])
          fieldRef = return (\name e -> ReferenceExpression $ FieldReference e name)
          indexRef = brackets $ (\indexes name e -> ReferenceExpression $ IndexReference (FieldReference e name) indexes) <$> withSourcePos expr `sepBy` comma
          methodCall = parens $ (\args name e -> CallExpression $ MethodCall e name args) <$> withSourcePos expr `sepBy` comma

maybeAssignment :: Parser Expression
maybeAssignment = do
    e <- maybeFieldAccess
    pos <- getPosition
    case e of
        ReferenceExpression ref -> choice
            [ AssignExpression ref <$ staticToken AssignToken <*> withSourcePos expr
            , AssignExpression ref <$ staticToken AssignAddToken <*> synthOp pos NumberAddExpression ref (withSourcePos expr)
            , AssignExpression ref <$ staticToken AssignSubtractToken <*> synthOp pos NumberSubtractExpression ref (withSourcePos expr)
            , AssignExpression ref <$ staticToken AssignMultiplyToken <*> synthOp pos NumberMultiplyExpression ref (withSourcePos expr)
            , AssignExpression ref <$ staticToken AssignDivideToken <*> synthOp pos NumberDivideExpression ref (withSourcePos expr)
            , AssignExpression ref <$ staticToken AssignModuloToken <*> synthOp pos NumberModuloExpression ref (withSourcePos expr)
            , AssignExpression ref <$ staticToken IncrementToken <*> synthOpOne pos NumberAddExpression ref
            , AssignExpression ref <$ staticToken DecrementToken <*> synthOpOne pos NumberSubtractExpression ref
            , return e
            ]
        _ -> return e
    where one = NumberLiteralExpression "1"
          synthOp pos op ref rhs = WithSourcePos pos . (WithSourcePos pos (ReferenceExpression ref) `op`) <$> rhs
          synthOpOne pos op ref = synthOp pos op ref $ return $ WithSourcePos pos one

opTable :: SourcePos -> OperatorTable [(SourcePos, Token)] TSState Identity Expression
opTable beforePos = [ [prefix InvertToken BoolInvertExpression
                      ,prefix SubtractToken (NumberSubtractExpression $ WithSourcePos beforePos $ NumberLiteralExpression "0")
                      ]
                    , [binLeft MultiplyToken NumberMultiplyExpression, binLeft DivideToken NumberDivideExpression, binLeft ModuloToken NumberModuloExpression]
                    , [binLeft AddToken NumberAddExpression, binLeft SubtractToken NumberSubtractExpression]
                    , [binLeft AppendToken StringAppendExpression
                      ,binLeft SpcKeyword (strBetween " ")
                      ,binLeft TabKeyword (strBetween "\t")
                      ,binLeft NlKeyword (strBetween "\r\n")
                      ]
                    , [binLeft NumEqualsToken NumberEqualsExpression
                      ,binLeft NumNoEqualsToken NumberNoEqualsExpression
                      ,binLeft LessThanToken NumberLessThanExpression
                      ,binLeft LessThanOrEqualsToken NumberLessThanOrEqualsExpression
                      ,binLeft GreaterThanToken NumberGreaterThanExpression
                      ,binLeft GreaterThanOrEqualsToken NumberGreaterThanOrEqualsExpression
                      ,binLeft StrEqualsToken StringEqualsExpression
                      ,binLeft StrNoEqualsToken StringNoEqualsExpression
                      ]
                    , [binLeft AndToken BoolAndExpression]
                    , [binLeft OrToken BoolOrExpression]
                    ]
    where binary opToken func = Infix $ do
              _ <- staticToken opToken
              afterPos <- getPosition
              return (\a b -> func (WithSourcePos beforePos a) (WithSourcePos afterPos b))
          binLeft opToken func = binary opToken func AssocLeft
          strBetween addBetween a b = a `StringAppendExpression` WithSourcePos beforePos (WithSourcePos beforePos (StrLiteralExpression addBetween) `StringAppendExpression` b)
          prefix opToken func = Prefix (func . WithSourcePos beforePos <$ staticToken opToken)

ternaryTerm :: Parser Expression
ternaryTerm = join (buildExpressionParser <$> (opTable <$> getPosition) <*> return maybeAssignment)

ternaryExpr :: Parser Expression
ternaryExpr = TernaryExpression
          <$> try (withSourcePos ternaryTerm
          <*  staticToken QuestionMarkToken)
          <*> withSourcePos ternaryTerm
          <*  staticToken SingleColonToken
          <*> withSourcePos expr

expr :: Parser Expression
expr = ternaryExpr <|> ternaryTerm

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
         <$> withSourcePos expr
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

whileStatement :: Parser Statement
whileStatement = staticToken WhileKeyword
              *> (WhileStatement
             <$> parens (withSourcePos expr)
             <*> blockOrStatement)

doWhileStatement :: Parser Statement
doWhileStatement = staticToken DoKeyword
                *> (flip DoWhileStatement
               <$> blockOrStatement
               <*> parens (withSourcePos expr)
               <*  semicolon)

returnStatement :: Parser Statement
returnStatement = staticToken ReturnKeyword
               *> (ReturnStatement
              <$> optionMaybe (withSourcePos expr))
              <*  semicolon

statement :: Parser Statement
statement = choice
          [ ExprStatement <$> withSourcePos expr <* semicolon
          , ifStatement
          , switchStatement
          , forStatement
          , whileStatement
          , doWhileStatement
          , returnStatement
          , BreakStatement <$ staticToken BreakKeyword <* semicolon
          , ContinueStatement <$ staticToken ContinueKeyword <* semicolon
          ]

block :: Parser Block
block = braces $ many $ withSourcePos statement

blockOrStatement :: Parser Block
blockOrStatement = block <|> ((: []) <$> withSourcePos statement)

function :: Parser Function
function = staticToken FunctionKeyword
        *> (Function
       <$> functionNameToken
       <*> argList
       <*> block)
    where arguments = localVarToken `sepBy` comma
          argList = parens arguments

functionDef :: Parser Definition
functionDef = FunctionDef <$> function

package :: Parser Package
package = staticToken PackageKeyword
       *> (Package
      <$> nameToken
      <*> braces (many $ withSourcePos function)
      <*  semicolon)

packageDef :: Parser Definition
packageDef = PackageDef <$> package

definition :: Parser Definition
definition = choice
           [ functionDef
           , packageDef
           ]

topLevel :: Parser TopLevel
topLevel = choice
    [ TopLevelDef <$> withSourcePos definition
    , TopLevelStatement <$> withSourcePos statement
    ]

parseTokens :: SourceName -> [(SourcePos, Token)] -> Either ParseError [TopLevel]
parseTokens = parse (many topLevel <* eof)

parseTS :: SourceName -> Text -> Either ParseError [TopLevel]
parseTS name input = traceIt <$> tokenize name input >>= parseTokens name
    where
#ifdef FLAG_DEBUG
          traceIt = fmap traceShowId
#else
          traceIt = id
#endif
