module CMacroParser where

import Control.Applicative hiding ((<|>),many)
import Data.List
import Text.Parsec
import Text.Parsec.String

import CTokenParser
import CParseTypes
import Common

macroChar :: Parser Char
macroChar = alphaNum <|> char '_'

showArgList :: Maybe [String] -> String
showArgList Nothing = ""
showArgList (Just args) = '(' : (concat $ intersperse ", " args) ++ ")"

defineArgsParser :: Parser (Maybe [String])
defineArgsParser = Just <$> between (char '(') (char ')') defineArgsParser' <|> return Nothing
    where defineArgsParser' = m_commaSep m_identifier

macroArgsParser :: Parser (Maybe [String])
macroArgsParser = Just <$> between (char '(') (char ')') macroArgsParser' <|> return Nothing
  where macroArgsParser' = macroArg `sepBy` (char ',' >> m_whiteSpace)
        macroArg = concat <$> many argPeice
        argPeice = try arg_parens <|> (many1 $ noneOf "(),")
        enclosedArgPeice = try arg_parens <|> (many1 $ noneOf "()")
        arg_parens = do open <- char '('
                        body <- concat <$> many enclosedArgPeice
                        close <- char ')'
                        return $ open : body ++ [close]

macroParser :: [MacroDef] -> Parser CodeSegment
macroParser md = do (text, (mac, argList)) <- includeConsumed possibleMacro
                    case find ((mac==) . title) md of
                        Nothing -> fail "Macro does not exist"
                        Just m -> if (length <$> argList) == parameters m then
                                      return $ CodeSegment text $ Macro argList (redefine m) else
                                      return $ CodeSegment text $ ErrorSegment "Incorrect number of arguments"
    where possibleMacro = do
              mac <- many1 macroChar
              argList <- macroArgsParser
              return (mac, argList)


