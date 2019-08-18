module CPreprocessor where

import Control.Applicative hiding ((<|>),many)
import Control.Arrow ((***))
import Control.Monad
import Control.Monad.Trans
import Data.List
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Pos
import System.FilePath

import CLineParser
import CParseTypes
import CTokenParser
import CMacroParser
import Common

maybeToList :: Maybe [a] -> [a]
maybeToList Nothing = []
maybeToList (Just xs) = xs

preprocessParser :: [String] -> [MacroDef] -> Parser [CodeSegment]
preprocessParser phs md = many1 $ codeSegmentParser phs md

codeSegmentParser :: [String] -> [MacroDef] -> Parser CodeSegment
codeSegmentParser phs md = try (includeConsumed m_stringLiteral >>= \(s, _) ->
                               return $ CodeSegment s Plain)  <|>
                           try ((includeConsumed $ placeHolderParser phs) >>= \(s, ph) ->
                               return $ CodeSegment s ph)  <|>
                           try (macroParser md) <|>
                           try (many1 macroChar >>= \s -> 
                               return $ CodeSegment s Plain) <|>
                           (anyChar >>= \c ->
                               return $ CodeSegment (c:[]) Plain)

placeHolderParser :: [String] -> Parser CodeInfo
placeHolderParser phs = m_identifier >>=
                        \id -> case id `elemIndex` phs of
                                   Nothing -> fail "Not a placeholder"
                                   Just n -> return $ Placeholder n

showArgList :: Maybe [String] -> String
showArgList Nothing = ""
showArgList (Just args) = '(' : (concat $ intersperse ", " args) ++ ")"
                    
preprocessWithPlaceHolders :: Maybe [String] -> [MacroDef] -> String -> [CodeSegment]
preprocessWithPlaceHolders ph md s = case parse (preprocessParser phs md) "" s of
                       Left err -> fail $ show err
                       Right ans -> concatPlain ans 
    where
      phs = maybeToList ph
      concatPlain = foldr acc []
      acc (CodeSegment t1 Plain) (CodeSegment t2 Plain : rest) = CodeSegment (t1 ++ t2) Plain : rest
      acc x xs = x : xs

preprocessStr :: [MacroDef] -> String -> [CodeSegment]
preprocessStr = preprocessWithPlaceHolders Nothing

preprocessLines :: String -> 
                   ParsecT [(String, CLine)] PreprocessState IO (PreprocessState, [[CodeSegment]])
preprocessLines filepath = do cs <- many codeSegment
                              st <- getState
                              return (st, cs)
    where codeSegment = try (
                          do (text, f) <- includePrim 
                             (st, segs) <- lift $ processInclude f 
                             setState st
                             return $ [CodeSegment text $ IncludeSegment segs]
                            )
                        <|>
                        try (
                          do (text, d) <- definePrim 
                             st <- getState
                             let newState = addMacroDef st $ defineToMacroDef (macroDefs st) d in
                                 putState newState
                             return $ preprocessStr (macroDefs st) text
                             --return $ [DirectiveSegment $ preprocessStr (macroDefs st) text]
                            )
                        <|>
                        do (text, _) <- codePrim
                           st <- getState
                           return $ preprocessStr (macroDefs st) text
              where 
                  includePrim = tokenPrim printText incPosition test 
                      where test (text, DirectiveLine (Include f)) = Just (text, f)
                            test _ = Nothing
                  definePrim = tokenPrim printText incPosition test
                      where test (text, (DirectiveLine d@(Define _ _ _))) = Just (text, d)
                            test _ = Nothing
                  codePrim = tokenPrim printText incPosition test
                      where test (text, CodeLine) = Just (text, CodeLine)
                            test _ = Nothing
                  incPosition pos _ _ = incSourceColumn pos (sourceColumn pos)  
                  printText (s, ln) = s
                  processInclude (QuoteFile f) = preprocessFile $ takeDirectory filepath </> f
                      

defineToMacroDef :: [MacroDef] -> Directive -> MacroDef
defineToMacroDef md (Define s p r) = MacroDef s (length <$> p) $ preprocessWithPlaceHolders p md r
defineToMacroDef _ _ = error "Not a define directive"

showOriginal :: [CodeSegment] -> String
showOriginal = concatMap text          

--showPreprocessed :: [String] -> [CodeSegment] -> String
--showPreprocessed phs = foldr appendSegment ""
--    where appendSegment seg [] = expandSegment seg
--          appendSegment seg rest = expandSegment seg ++ " " ++ rest
--          expandSegment (Plain s) = s
--          expandSegment (DirectiveSegment segments) = '\n' : (showPreprocessed phs segments) ++ "\n"
--          expandSegment (Placeholder n) = phs !! n
--          expandSegment (Macro _ ph rd) = showPreprocessed (maybeToList ph ++ phs) rd
--          expandSegment (IncludeSegment _ segments) = showPreprocessed [] segments
--          expandSegment (ErrorSegment s _) = s


preprocess :: String -> String -> IO (PreprocessState, [CodeSegment])
preprocess filepath content = do res <- runParserT (preprocessLines filepath) (mempty :: PreprocessState)  "" $ lineParser content
                                 case res of
                                     Left _ -> error "Parse error"
                                     Right r -> return $ (id *** concat) (r :: (PreprocessState, [[CodeSegment]]))

preprocessFile :: String -> IO (PreprocessState, [CodeSegment])
preprocessFile filepath = do
    content <- readFile filepath
    preprocess filepath content
