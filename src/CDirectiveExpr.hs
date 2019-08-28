module CDirectiveExpr where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import CTokenParser

data Expr = Constant Int | Duo Duop Expr Expr
    deriving Show

data Duop = Times | Plus
    deriving Show

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [Infix (m_reservedOp "*" >> return (Duo Times)) AssocLeft]
        , [Infix (m_reservedOp "+" >> return (Duo Plus)) AssocLeft]
        ]
term = m_parens exprparser
       <|> (m_reserved "true" >> return (Constant 1))
       <|> (m_identifier >> return (Constant 0))
