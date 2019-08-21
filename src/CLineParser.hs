module CLineParser where

import Control.Applicative hiding ((<|>),many)
import Control.Arrow
import Data.List
import Text.Parsec
import Text.Parsec.String

import CTokenParser
import CParseTypes
import CMacroParser

showIncludeFile :: IncludeFile -> String
showIncludeFile (AngleBracketFile f) = '<' : f ++ ">"
showIncludeFile (QuoteFile f) = '"' : f ++ "\""

line :: Parser CLine
line = try $ DirectiveLine <$> directiveParser <|> return CodeLine <* many anyChar

restOfLine :: Parser String
restOfLine = try (manyTill anyChar endOfLine <* endOfLine) <|> (many anyChar <* eof)

directiveParser :: Parser Directive
directiveParser = include <|>
                  define  <|>
                  ifdef   <|>
                  ifndef  <|>
                  endif

include = m_reserved "#include" >> (angleBracketFile <|> quoteFile)

angleBracketFile = do f <- m_angles (many $ noneOf "<>") 
                      return (Include $ AngleBracketFile f)

quoteFile = do f <- m_stringLiteral
               return (Include $ QuoteFile f)

define = do m_reserved "#define"
            d <- many $ macroChar
            args <- (defineArgsParser <|> return Nothing)
            m_whiteSpace
            r <- ( try (manyTill anyChar endOfLine) <|> many anyChar)
            return $ Define d args r

ifdef = m_reserved "#ifdef" >> Ifdef <$> m_identifier

ifndef = m_reserved "#ifndef" >> Ifndef <$> m_identifier

endif = m_reserved "#endif" >> return Endif

mainParser :: Parser CLine
mainParser = m_whiteSpace >> line <* eof

zipLines :: [String] -> [Either ParseError CLine] -> Either [ParseError] [(String, CLine)]
zipLines lns xs = ret $ foldr checkLine ([],[]) xs
    where checkLine (Left err) acc = ((err:)***id) acc
          checkLine (Right x) acc = (id***(x:)) acc
          ret ([], xs) = Right $ zip lns xs
          ret (errs, []) = Left errs

concatLines :: String -> [(String, CLine)] -> [(String, CLine)]
concatLines sep lines = foldr acc [last lines] $ init lines
    where acc ln@(_, DirectiveLine _) lns = ln:lns
          acc (s, CodeLine) ((ss, CodeLine):lns) = (s ++ sep ++ ss, CodeLine) : lns
          acc ln@(_, CodeLine) lns = ln : lns

endLines :: String -> [(String, CLine)] -> [(String, CLine)]
endLines sep lines = (map (\(s, cl) -> (s ++ sep, cl)) $ init lines) ++ [last lines]

parseCFileContent :: String -> [Either ParseError CLine]
parseCFileContent inp = let lns = lines inp in
                            map (parse mainParser "" ) lns

lineParser :: String -> [(String, CLine)]
lineParser s = case zipLines (lines s) $ parseCFileContent s of
                   Left _ -> fail "Parse error"
                   Right lns -> endLines "\n" $ concatLines "\n" lns
