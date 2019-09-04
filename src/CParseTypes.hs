module CParseTypes where

data CLine = DirectiveLine Directive | CodeLine
    deriving Show

data Directive = Include IncludeFile 
               | Define String (Maybe [String]) String
               | Undef String
               | Ifdef String
               | Ifndef String
               | If String
               | Elif String
               | Else
               | Endif
               | ErrorDirective String
    deriving Show

data IncludeFile = AngleBracketFile String | QuoteFile String
    deriving Show

data CodeInfo =    Plain
                 | Macro (Maybe [String]) [CodeSegment]
                 | Placeholder Int
                 | IncludeSegment [CodeSegment]
                 | Conditional [(Bool, [CodeSegment])]
                 | ErrorSegment ErrorMsg
    deriving Show

data MacroDef = MacroDef { title :: String
                         , parameters :: Maybe Int
                         , redefine :: [CodeSegment]
                         }
    deriving Show

data CodeSegment = CodeSegment { text :: String
                               , info :: CodeInfo
                               }
    deriving Show

data PreprocessState = PreprocessState { macroDefs :: [MacroDef]
                                       , includeDirs :: [FilePath]
                                       }
    deriving Show

instance Semigroup PreprocessState where
    (<>) (PreprocessState md1 id1) (PreprocessState md2 id2) = PreprocessState (md1++md2) (id1++id2)

instance Monoid PreprocessState where
    mempty = PreprocessState [] []

emptyState :: PreprocessState
emptyState = PreprocessState [] []

startState :: PreprocessState
startState = addIncludeDirList emptyState ["/usr/local/include", "/usr/include", "/usr/include/x86_64-linux-gnu", "/usr/lib/gcc/x86_64-linux-gnu/8/include", "/usr/lib/gcc/x86_64-linux-gnu/8/include-fixed"]

addMacroDef :: PreprocessState -> MacroDef -> PreprocessState
addMacroDef st md = st { macroDefs = (md : macroDefs st) }

addIncludeDirList :: PreprocessState -> [FilePath] -> PreprocessState
addIncludeDirList st inc = st { includeDirs = includeDirs st ++ inc }

type ErrorMsg = String
