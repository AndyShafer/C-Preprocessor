module Common where

import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.String
import CTokenParser
import CParseTypes
import Data.List

-- Returns the result of the parser and the entire string that was consumed.
includeConsumed :: Parser a -> Parser (String, a)
includeConsumed p = do startPos <- getPosition
                       (x, endPos) <- lookAhead p'
                       text <- substringParser startPos endPos
                       return (text, x)
    where 
        p' = do r <- p
                e <- getPosition
                return (r, e)

replaceAll rep before = option "" (try rep) >>= \begin ->
                        (manyTill $ do
                           (unreplaced, _) <- includeConsumed $ manyTill anyChar $ (eof <|> (ignoreResult $ lookAhead before))
                           (bef, _) <- includeConsumed $ option "" (try before)
                           replaced <- option "" (try rep)
                           return $ unreplaced ++ bef ++ replaced) eof >>= \res -> return $ begin ++ (concat res)

ignoreResult p = p >> return ()

anythingBut :: Parser Char -> Parser Char
anythingBut p = notFollowedBy p >> anyChar

-- Parses and returns string between start and end positions.
-- The start position cannot be before the current position.
substringParser :: SourcePos -> SourcePos -> Parser String
substringParser start end = do many $ notPos start
                               many $ notPos end
    where 
        notPos :: SourcePos -> Parser Char
        notPos pos = do p <- getPosition
                        if p == pos then
                            fail "Reached end" else
                            anyChar

restOfLine :: Parser String
restOfLine = manyTill anyCharSkipComment ((endOfLine >> return ()) <|> eof)

restOfLineWithExtensions :: Parser String
restOfLineWithExtensions = (concat . intersperse " ") <$>
             sepBy restOfLineWithExtensions' lineContinue

restOfLineWithExtensions' :: Parser String
restOfLineWithExtensions' = manyTill anyCharSkipComment
                       ((try $ lookAhead lineContinue >> return ()) <|> (endOfLine >> return ()) <|> eof)

lineContinue :: Parser Char
lineContinue = char '\\' >> endOfLine >> return ' '

-- Parse any char but if a comment is found comsume it and return a space.
anyCharSkipComment :: Parser Char
anyCharSkipComment = try commentParser <|> anyChar

commentParser :: Parser Char
commentParser = try (
                  do string "/*"
                     manyTill anyChar $ string "*/"
                     return ' '
                    ) <|>
                  do string "//"
                     manyTill anyChar (lookAhead (endOfLine >> return ()) <|> eof)
                     return ' '

removeWhiteSpaceBefore :: String -> String
removeWhiteSpaceBefore s = case parse (m_whiteSpace >> many anyChar) "" s of
                              Right a -> a

removeWhiteSpaceAfter :: String -> String
removeWhiteSpaceAfter = reverse . removeWhiteSpaceBefore . reverse

isDefined :: [MacroDef] -> String -> Bool
isDefined mds s = case find ((s==) . title) mds of
                       Nothing -> False
                       Just _ -> True

