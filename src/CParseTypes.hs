module CParseTypes where

data CLine = DirectiveLine Directive | CodeLine
    deriving Show

data Directive = Include IncludeFile 
               | Define String (Maybe [String]) String
               | Ifdef String
               | Ifndef String
               | Endif
    deriving Show

data IncludeFile = AngleBracketFile String | QuoteFile String
    deriving Show

data CodeSegment = Plain String 
                 | Macro String (Maybe [String]) [CodeSegment]
                 | Placeholder Int
                 | DirectiveSegment [CodeSegment]
                 | IncludeSegment String [CodeSegment]
                 | Conditional Bool [CodeSegment]
                 | ErrorSegment String ErrorMsg
    deriving Show

data MacroDef = MacroDef { title :: String
                         , parameters :: Maybe Int
                         , redefine :: [CodeSegment]
                         }
    deriving Show

data PreprocessState = PreprocessState { macroDefs :: [MacroDef] }
    deriving Show

instance Semigroup PreprocessState where
    (<>) (PreprocessState x1) (PreprocessState x2) = PreprocessState (x1++x2)

instance Monoid PreprocessState where
    mempty = PreprocessState []

emptyState :: PreprocessState
emptyState = PreprocessState []

addMacroDef :: PreprocessState -> MacroDef -> PreprocessState
addMacroDef st md = st { macroDefs = (md : macroDefs st) }

type ErrorMsg = String
