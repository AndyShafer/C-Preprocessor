module CLineParser where

import Control.Applicative hiding ((<|>),many)
import Control.Arrow
import Data.List
import Text.Parsec
import Text.Parsec.String

import CTokenParser
import CParseTypes
import CMacroParser
import Common

showIncludeFile :: IncludeFile -> String
showIncludeFile (AngleBracketFile f) = '<' : f ++ ">"
showIncludeFile (QuoteFile f) = '"' : f ++ "\""

line :: Parser CLine
line = (lookAhead anyChar) >> (try $ DirectiveLine <$> directiveParser <|> return CodeLine <* restOfLine)

--restOfLine :: Parser String
--restOfLine = sepBy
--                 (try (manyTill anyChar endOfLine <* endOfLine) <|> (many anyChar <* eof))
--                 lineContinue


restOfLine :: Parser String
restOfLine = manyTill anyChar ((endOfLine >> return ()) <|> eof)

restOfLineWithExtentions :: Parser String
restOfLineWithExtentions = (concat . intersperse " ") <$>
             sepBy restOfLineWithExtentions' lineContinue

restOfLineWithExtentions' :: Parser String
restOfLineWithExtentions' = manyTill anyChar
                       ((try $ lookAhead lineContinue >> return ()) <|> (endOfLine >> return ()) <|> eof)

lineContinue :: Parser Char
lineContinue = char '\\' >> endOfLine >> return ' '

directiveParser = char '#' >> m_whiteSpace >>
                      (include       <|>
                      define         <|>
                      ifDirective    <|>
                      elif           <|>
                      ifdef          <|>
                      ifndef         <|>
                      elseDirective  <|>
                      endif)

include = m_reserved "include" >> (angleBracketFile <|> quoteFile)

angleBracketFile = do f <- m_angles (many $ noneOf "<>") 
                      return (Include $ AngleBracketFile f)

quoteFile = do f <- m_stringLiteral
               return (Include $ QuoteFile f)

define = do m_reserved "define"
            d <- many $ macroChar
            args <- (defineArgsParser <|> return Nothing)
            m_whiteSpace
            r <- restOfLineWithExtentions
            return $ Define d args r

ifdef = m_reserved "ifdef" >> Ifdef <$> m_identifier

ifDirective = m_reserved "if" >> If <$> ( try (manyTill anyChar endOfLine) <|> many anyChar)

elif = m_reserved "elif" >> Elif <$> ( try (manyTill anyChar endOfLine) <|> many anyChar)

ifndef = m_reserved "ifndef" >> Ifndef <$> m_identifier

endif = m_reserved "endif" >> return Endif

elseDirective = m_reserved "else" >> return Else

mainParser :: Parser [(String, CLine)]
mainParser = many (includeConsumed line) <* eof

zipLines :: [String] -> [Either ParseError CLine] -> Either [ParseError] [(String, CLine)]
zipLines lns xs = ret $ foldr checkLine ([],[]) xs
    where checkLine (Left err) acc = ((err:)***id) acc
          checkLine (Right x) acc = (id***(x:)) acc
          ret ([], xs) = Right $ zip lns xs
          ret (errs, _) = Left errs

concatLines :: String -> [(String, CLine)] -> [(String, CLine)]
concatLines sep lines = foldr acc [last lines] $ init lines
    where acc ln@(_, DirectiveLine _) lns = ln:lns
          acc (s, CodeLine) ((ss, CodeLine):lns) = (s ++ sep ++ ss, CodeLine) : lns
          acc ln@(_, CodeLine) lns = ln : lns

parseCFileContent :: String -> Either ParseError [(String, CLine)]
parseCFileContent inp = parse mainParser "" inp

lineParser :: String -> [(String, CLine)]
lineParser s = case parseCFileContent s of
                   Left _ -> fail "Parse error"
                   Right lns -> concatLines "" lns
