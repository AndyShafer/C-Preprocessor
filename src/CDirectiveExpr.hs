module CDirectiveExpr where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import CTokenParser

data Expr = Constant Int | Uno Unop Expr | Duo Duop Expr Expr
    deriving Show

data Duop = Times | Divide | Mod |
            Plus | Minus | 
            ShiftL | ShiftR |
            Less | Greater | LTEQ | GTEQ | 
            Equal | NotEqual | 
            BitAnd |
            BitXor |
            BitOr |
            And |
            Or
    deriving Show

data Unop = Not | Pos | Neg | BitNot
    deriving Show

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [prefixOp "+" Pos
          ,prefixOp "-" Neg
          ,prefixOp "!" Not
          ,prefixOp "~" BitNot]
        , [infixOp "*" Times
          ,infixOp "/" Divide
          ,infixOp "%" Mod]
        , [infixOp "+" Plus
          ,infixOp "-" Minus]
        , [infixOp "<<" ShiftL
          ,infixOp ">>" ShiftR]
        , [infixOp "<" Less
          ,infixOp ">" Greater
          ,infixOp "<=" LTEQ
          ,infixOp ">=" GTEQ]
        , [infixOp "==" Equal
          ,infixOp "!=" NotEqual]
        , [infixOp "&" BitAnd]
        , [infixOp "^" BitXor]
        , [infixOp "|" BitOr]
        , [infixOp "&&" And]
        , [infixOp "||" Or]
        ]
term = m_parens exprparser
       <|> (m_reserved "true" >> return (Constant 1))
       <|> (m_identifier >> return (Constant 0))

infixOp op duop = Infix (m_reservedOp op >> return (Duo duop)) AssocLeft

prefixOp op unop = Prefix (m_reservedOp op >> return (Uno unop))
