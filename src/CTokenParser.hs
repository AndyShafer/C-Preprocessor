module CTokenParser where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

def = emptyDef{ commentStart = "/*"
            , commentEnd = "*/"
            , commentLine = "//"
            , identStart = letter <|> char '_'
            , identLetter = alphaNum <|> char '_'
            , reservedNames = ["include", "define", "ifdef", "ifndef", "else", "endif", "true"]
            , reservedOpNames = ["+", "*", "-", "/", "!", "&", "|", "~", "^", "&&", "||",
                                 "==", "!=", ">", "<", ">=", "<=", "<<", ">>", "%"]
            , caseSensitive = True
            }

TokenParser{ parens = m_parens
           , angles = m_angles
           , commaSep = m_commaSep
           , stringLiteral = m_stringLiteral
           , reserved = m_reserved
           , reservedOp = m_reservedOp
           , identifier = m_identifier
           , whiteSpace = m_whiteSpace } = makeTokenParser def
