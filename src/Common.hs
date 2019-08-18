module Common where

import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.String

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

