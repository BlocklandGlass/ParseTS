module Language.TorqueScript.Tokens where

data Token = StrToken String
           | NumToken String
           | NameToken String
           | GlobalVarToken String
           | LocalVarToken String
           -- Keywords
           | FunctionKeyword
           | PackageKeyword
           | ReturnKeyword
           | TrueKeyword
           | FalseKeyword
           | IfKeyword
           | ElseKeyword
           | ForKeyword
           | WhileKeyword
           | DoKeyword
           | BreakKeyword
           | ContinueKeyword
           | SwitchKeyword
           | StrSwitchKeyword
           | CaseKeyword
           | DefaultKeyword
           | TabKeyword
           | SpcKeyword
           | NlKeyword
           | NewKeyword
           -- Parens
           | ParenBeginToken
           | ParenEndToken
           | BraceBeginToken
           | BraceEndToken
           | BracketBeginToken
           | BracketEndToken
           -- Punctuation
           | SemicolonToken
           | CommaToken
           | AssignToken
           | QuestionMarkToken
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
           | IncrementToken
           | DecrementToken
           -- Bool operations
           | InvertToken
           | OrToken
           | AndToken
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