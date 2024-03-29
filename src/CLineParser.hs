module CLineParser where

import Control.Applicative hiding ((<|>),many)
import Control.Arrow
import Data.List
import Text.Parsec
import Text.Parsec.String

import CTokenParser
import CParseTypes
import CMacroParser (macroChar, defineArgsParser)
import Common

showIncludeFile :: IncludeFile -> String
showIncludeFile (AngleBracketFile f) = '<' : f ++ ">"
showIncludeFile (QuoteFile f) = '"' : f ++ "\""

line :: Parser CLine
line = (lookAhead anyChar) >> (try $ DirectiveLine <$> directiveParser <|> return CodeLine <* restOfLine)

parseDirectiveContent :: Parser a -> Parser a
parseDirectiveContent parser = do content <- restOfLineWithExtensions
                                  case parse (m_whiteSpace >> parser) "" content of
                                       Left _ -> fail "Could not parse directive content"
                                       Right res -> return res

directiveParser = char '#' >> m_whiteSpace >>
                      (include       <|>
                      define         <|>
                      undef          <|>
                      ifDirective    <|>
                      elif           <|>
                      ifdef          <|>
                      ifndef         <|>
                      elseDirective  <|>
                      endif          <|>
                      errorDirective <|>
                      warningDirective)

include = m_reserved "include" >> parseDirectiveContent (angleBracketFile <|> quoteFile)

angleBracketFile = do f <- m_angles (many $ noneOf "<>") 
                      return (Include $ AngleBracketFile f)

quoteFile = do f <- m_stringLiteral
               return (Include $ QuoteFile f)

define = do m_reserved "define"
            d <- many $ macroChar
            args <- (defineArgsParser <|> return Nothing)
            m_whiteSpace
            r <- restOfLineWithExtensions
            return $ Define d args r

undef = m_reserved "undef" >> parseDirectiveContent (Undef <$> m_identifier)

ifdef = m_reserved "ifdef" >> parseDirectiveContent (Ifdef <$> m_identifier)

ifDirective = m_reserved "if" >> parseDirectiveContent (If <$> many anyChar)

elif = m_reserved "elif" >> parseDirectiveContent (Elif <$> many anyChar)

ifndef = m_reserved "ifndef" >> parseDirectiveContent (Ifndef <$> m_identifier)

endif = m_reserved "endif" >> return Endif 

elseDirective = m_reserved "else" >> return Else

errorDirective = m_reserved "error" >> parseDirectiveContent (ErrorDirective <$> m_stringLiteral)

warningDirective = m_reserved "warning" >> parseDirectiveContent (WarningDirective <$> m_stringLiteral)

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
