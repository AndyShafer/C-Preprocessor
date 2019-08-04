module CParseTypes where

data CLine = DirectiveLine Directive | CodeLine
    deriving Show

data Directive = Include IncludeFile | Define String (Maybe [String]) String
    deriving Show

data IncludeFile = AngleBracketFile String | QuoteFile String
    deriving Show

data CodeSegment = Plain String 
                 | Macro String (Maybe [String]) [CodeSegment]
                 | Placeholder Int
                 | DirectiveSegment [CodeSegment]
                 | IncludeSegment String [CodeSegment]
                 | ErrorSegment String ErrorMsg
    deriving Show

data MacroDef = MacroDef { title :: String
                         , parameters :: Maybe Int
                         , redefine :: [CodeSegment]
                         }
              | MacroFunc
    deriving Show

type ErrorMsg = String
