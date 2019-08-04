module CPreprocessor where

import Control.Applicative hiding ((<|>),many)
import Control.Arrow ((***))
import Control.Monad
import Data.List
import Text.Parsec
import Text.Parsec.String
import System.FilePath

import CLineParser
import CParseTypes
import CTokenParser
import CMacroParser

maybeToList :: Maybe [a] -> [a]
maybeToList Nothing = []
maybeToList (Just xs) = xs

preprocessParser :: [String] -> [MacroDef] -> Parser [CodeSegment]
preprocessParser phs md = many1 $ codeSegmentParser phs md

codeSegmentParser :: [String] -> [MacroDef] -> Parser CodeSegment
codeSegmentParser phs md = (try $ m_stringLiteral >>= \s -> return $ Plain $ '"' : s ++ "\"") <|>
                           (try $ placeHolderParser phs) <|>
                           (try $ macroParser md) <|>
                           (Plain <$> many1 macroChar) <|>
                           ((Plain . (:[])) <$> anyChar)

placeHolderParser :: [String] -> Parser CodeSegment
placeHolderParser phs = m_identifier >>=
                        \id -> case id `elemIndex` phs of
                                   Nothing -> fail "Not a placeholder"
                                   Just n -> return $ Placeholder n

showArgList :: Maybe [String] -> String
showArgList Nothing = ""
showArgList (Just args) = '(' : (concat $ intersperse ", " args) ++ ")"
                    
checkIfMacro :: [MacroDef] -> String -> CodeSegment
checkIfMacro mds s = case find ((s==) . title) mds of
                        Just md -> Macro (title md) Nothing (redefine md)
                        Nothing -> Plain s

preprocessWithPlaceHolders :: Maybe [String] -> [MacroDef] -> String -> [CodeSegment]
preprocessWithPlaceHolders ph md s = case parse (preprocessParser phs md) "" s of
                       Left err -> fail $ show err
                       Right ans -> concatPlain ans 
    where
      phs = maybeToList ph
      concatPlain = foldr acc []
      acc (Plain p1) (Plain p2 : rest) = Plain (p1 ++ p2) : rest
      acc x xs = x : xs

preprocessStr :: [MacroDef] -> String -> [CodeSegment]
preprocessStr = preprocessWithPlaceHolders Nothing

preprocessLines :: String -> [(String, CLine)] -> IO ([MacroDef], [[CodeSegment]])
preprocessLines filepath lines = foldM acc ([], []) lines
    where
      acc (mds, lns) ln = do (mds', lns') <- handleLine mds ln
                             return $ ((mds'++)***(++lns')) (mds, lns)
      handleLine mds (text, (DirectiveLine (Include f))) =
          processInclude f >>= \(mds', lns') -> return (mds', [[DirectiveSegment [IncludeSegment text lns']]])
      handleLine mds (text, (DirectiveLine d)) = return (processDirective mds d, [preprocessStr mds text])
      handleLine mds (text, CodeLine) = return ([], [preprocessStr mds text])
      processDirective mds (Include _) = mds
      processDirective mds d@(Define _ _ _) = [defineToMacroDef mds d]
      processInclude (QuoteFile f) = preprocessFile $ takeDirectory filepath </> f
--    acc (mds, lns) (text, (DirectiveLine (Include f))) =
--        processInclude f >>=
--        \(mds', lns') -> (mds'++mds, lns ++ [[DirectiveSegment [IncludeSegment text lns']]])
--    acc (mds, lns) (text, (DirectiveLine d)) = let mds' = processDirective mds d in
--                                               (mds', lns ++ [[DirectiveSegment $ preprocessStr mds text]])
--    acc (mds, lns) (text, CodeLine) = (mds, lns ++ [preprocessStr mds text])
--    processDirective mds d@(Define _ _ _) = defineToMacroDef mds d : mds

defineToMacroDef :: [MacroDef] -> Directive -> MacroDef
defineToMacroDef md (Define s p r) = MacroDef s (length <$> p) $ preprocessWithPlaceHolders p md r
defineToMacroDef _ _ = error "Not a define directive"

showOriginal :: [CodeSegment] -> String
showOriginal = foldr acc ""
    where acc (Plain p) s = p ++ s
          acc (Macro m _ _) s = m ++ s

showPreprocessed :: [String] -> [CodeSegment] -> String
showPreprocessed phs = foldr appendSegment ""
    where appendSegment seg [] = expandSegment seg
          appendSegment seg rest = expandSegment seg ++ " " ++ rest
          expandSegment (Plain s) = s
          expandSegment (DirectiveSegment segments) = '\n' : (showPreprocessed phs segments) ++ "\n"
          expandSegment (Placeholder n) = phs !! n
          expandSegment (Macro _ ph rd) = showPreprocessed (maybeToList ph ++ phs) rd
          expandSegment (IncludeSegment _ segments) = showPreprocessed [] segments
          expandSegment (ErrorSegment s _) = s


preprocess :: String -> String -> IO ([MacroDef], [CodeSegment])
preprocess filepath content = (id***concat) <$> (preprocessLines filepath $ lineParser content)

preprocessFile :: String -> IO ([MacroDef], [CodeSegment])
preprocessFile filepath = do
    content <- readFile filepath
    preprocess filepath content

