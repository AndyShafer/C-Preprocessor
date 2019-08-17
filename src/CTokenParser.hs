module CTokenParser where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

def = emptyDef{ commentStart = "/*"
            , commentEnd = "*/"
            , commentLine = "//"
            , identStart = letter <|> char '_'
            , identLetter = alphaNum <|> char '_'
            , reservedNames = ["#include", "#define", "#ifdef", "#ifndef", "#endif"]
            , caseSensitive = True
            }

TokenParser{ angles = m_angles
           , commaSep = m_commaSep
           , stringLiteral = m_stringLiteral
           , reserved = m_reserved
           , identifier = m_identifier
           , whiteSpace = m_whiteSpace } = makeTokenParser def
