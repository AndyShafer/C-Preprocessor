module CTokenParser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Control.Applicative hiding ((<|>),many)

def = emptyDef{ commentStart = "/*"
            , commentEnd = "*/"
            , commentLine = "//"
            , identStart = letter <|> char '_'
            , identLetter = alphaNum <|> char '_'
            , reservedNames = ["include", "define", "ifdef", "ifndef", "else", "endif", "true"]
            , reservedOpNames = ["+", "*", "-", "/", "!", "&", "|", "~", "^", "&&", "||", "#",
                                 "==", "!=", ">", "<", ">=", "<=", "<<", ">>", "%", "defined"]
            , caseSensitive = True
            }

TokenParser{ parens = m_parens
           , angles = m_angles
           , commaSep = m_commaSep
           , stringLiteral = m_stringLiteral
           , reserved = m_reserved
           , reservedOp = m_reservedOp
           , operator = m_operator
           , identifier = m_identifier
           , whiteSpace = m_whiteSpace 
           , integer = m_integer } = makeTokenParser def

unsignedSuffix :: Parser String
unsignedSuffix = string "u"

longSuffix :: Parser String
longSuffix = (string "l" >>= \l1 -> option "" $ string "l" >>= \l2 -> return $ l1++l2) <|> 
             (string "L" >>= \l1 -> option "" $ string "L" >>= \l2 -> return $ l1++l2)

intSuffix :: Parser String
intSuffix = option "" $
              (do
                  u <- unsignedSuffix
                  l <- option "" longSuffix
                  return $ u++l) <|>
              (do
                  l <- longSuffix
                  u <- option "" unsignedSuffix
                  return $ l++u)

intParser :: Parser Integer
intParser = m_integer <* intSuffix <* m_whiteSpace
