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
import CDirectiveExpr
import Common

maybeToList :: Maybe [a] -> [a]
maybeToList Nothing = []
maybeToList (Just xs) = xs

isDefined :: [MacroDef] -> String -> Bool
isDefined mds s = case find ((s==) . title) mds of
                       Nothing -> False
                       Just _ -> True

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
                   ParsecT [(String, CLine)] PreprocessState IO (PreprocessState, [CodeSegment])
preprocessLines filepath = do cs <- many codeSegment
                              st <- getState
                              return (st, concat cs)
    where 
          codeSegment = try (
                          do (text, f) <- includePrim 
                             (st, segs) <- lift $ processInclude f 
                             setState st
                             return $ [CodeSegment text $ IncludeSegment segs]
                            ) <|>
                        try (
                          do (text, d) <- definePrim 
                             st <- getState
                             let newState = addMacroDef st $ defineToMacroDef (macroDefs st) d in
                                 putState newState
                             return $ preprocessStr (macroDefs st) text
                            ) <|>
                        try ifdef <|>
                        try ifndef <|>
                        try ifBlock <|>

                          do (text, _) <- codePrim
                             st <- getState
                             return $ preprocessStr (macroDefs st) text

          includePrim = tokenPrim printText incPosition test 
              where test (text, DirectiveLine (Include f)) = Just (text, f)
                    test _ = Nothing

          definePrim = tokenPrim printText incPosition test
              where test (text, (DirectiveLine d@(Define _ _ _))) = Just (text, d)
                    test _ = Nothing

          ifdefPrim = tokenPrim printText incPosition test
              where test (text, (DirectiveLine (Ifdef d))) = Just (text, d)
                    test _ = Nothing

          ifndefPrim = tokenPrim printText incPosition test
              where test (text, (DirectiveLine (Ifndef d))) = Just (text, d)
                    test _ = Nothing

          
          ifPrim = tokenPrim printText incPosition test
              where test (text, (DirectiveLine (If e))) = Just (text, e)
                    test _ = Nothing

          elifPrim = tokenPrim printText incPosition test
              where test (text, (DirectiveLine (Elif e))) = Just (text, e)
                    test _ = Nothing

          elsePrim = tokenPrim printText incPosition test
              where test (text, (DirectiveLine Else)) = Just (text, "")
                    test _ = Nothing

          endifPrim = tokenPrim printText incPosition test
              where test (text, (DirectiveLine Endif)) = Just (text, "")
                    test _ = Nothing

          codePrim = tokenPrim printText incPosition test
              where test (text, CodeLine) = Just (text, CodeLine)
                    test _ = Nothing

          incPosition pos _ _ = incSourceColumn pos (sourceColumn pos)  

          printText (s, ln) = s

          processInclude (QuoteFile f) = preprocessFile $ takeDirectory filepath </> f

          ifdef = ifdefBlock ifdefPrim id

          ifndef = ifdefBlock ifndefPrim not

          ifdefBlock start condMod = do
              (ifText, d) <- start
              st <- getState
              active <- return $ condMod $ isDefined (macroDefs st) d
              segs <- manyTill codeSegment (lookAhead $ elsePrim <|> endifPrim)
              ifSt <- getState
              setState st
              maybeElse <- optionMaybe elseBlock
              elseSt <- getState
              (elseText, elseInfo) <- return $ case maybeElse of
                                                 Just (elseT, elseS) -> (elseT, [(not active, elseS)])
                                                 Nothing -> ("", [])
              (endText, _) <- endifPrim
              setState (if active then ifSt else elseSt)
              return [CodeSegment
                      (ifText ++ (concat $ concat $ map (map text) segs) ++ elseText ++ endText) $
                      Conditional $ (active, concat segs):elseInfo]   

          ifBlock = do
              (ifText, e) <- ifPrim
              st <- getState
              active <- return $ (/= 0) $ evalExpr $ showPreprocessed [] $ preprocessStr (macroDefs st) e
              segs <- manyTill codeSegment (lookAhead $ elifPrim <|> elsePrim <|> endifPrim)
              ifSt <- getState
              elifs <- many (setState st >> elifBlock)
              setState st
              maybeElse <- optionMaybe elseBlock
              elseSt <- getState
              (endText, _) <- endifPrim
              setState (if active then ifSt else
                            case find (\(a, _, _, _) -> a) elifs of
                                Just (_, _, _, elifSt) -> elifSt
                                Nothing -> elseSt)
              elifs' <- return $ map (\(elifActive, elifSegs, elifT, _) ->
                                          (elifT, (elifActive, elifSegs))) elifs
              (elseText, elseInfo) <- return $ case maybeElse of
                                                 Just (elseT, elseS) -> (elseT, [(not active, elseS)])
                                                 Nothing -> ("", [])
              return [CodeSegment
                      (ifText ++ (concat $ concat $ map (map text) segs) ++
                          (concat $ map fst elifs') ++ elseText ++ endText) $
                      Conditional $ (active, concat segs) :
                          (map snd elifs') ++ elseInfo]   

          elifBlock = do
              (elifText, e) <- elifPrim
              st <- getState
              active <- return $ (/= 0) $ evalExpr $ showPreprocessed [] $ preprocessStr (macroDefs st) e
              segs <- manyTill codeSegment (lookAhead $ elifPrim <|> elsePrim <|> endifPrim)
              newSt <- getState
              return (active, concat segs, elifText ++ (concat $ concat $ map (map text) segs), newSt)

          elseBlock = do (elseText, _) <- elsePrim
                         segs <- manyTill codeSegment (lookAhead endifPrim)
                         return (elseText ++ (concat $ concat $ map (map text) segs), concat segs)
                      

defineToMacroDef :: [MacroDef] -> Directive -> MacroDef
defineToMacroDef md (Define s p r) = MacroDef s (length <$> p) $ preprocessWithPlaceHolders p md r
defineToMacroDef _ _ = error "Not a define directive"

showOriginal :: [CodeSegment] -> String
showOriginal = concatMap text          

showPreprocessed :: [String] -> [CodeSegment] -> String
showPreprocessed phs = concatMap expand
    where expand seg = case info seg of
                         Plain -> text seg
                         -- Append whitespace to ensure tokens are separated after expanding.
                         -- Might want to change concatMap to a foldr to we can check if the next
                         -- segment starts with whitespace.
                         Macro phs' segs -> showPreprocessed (maybeToList phs') segs ++ " "
                         Placeholder n -> phs !! n
                         IncludeSegment segs -> showPreprocessed [] segs
                         ErrorSegment msg -> text seg
                         Conditional groups -> case find (\(b, _) -> b) groups of
                                                   Just (_, code) -> showPreprocessed [] code
                                                   Nothing -> ""
                            
preprocess :: String -> String -> IO (PreprocessState, [CodeSegment])
preprocess filepath content = do res <- runParserT (preprocessLines filepath) (mempty :: PreprocessState)  "" $ lineParser content
                                 case res of
                                     Left _ -> error "Parse error"
                                     Right r -> return $ r

preprocessFile :: String -> IO (PreprocessState, [CodeSegment])
preprocessFile filepath = do
    content <- readFile filepath
    preprocess filepath content
