module CDirectiveExpr where

import Data.Bits
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import CTokenParser
import Common
import CParseTypes

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
       <|> (m_integer >>= \n -> return $ Constant $ fromIntegral n)
       <|> (m_identifier >> return (Constant 0))

infixOp op duop = Infix (m_reservedOp op >> return (Duo duop)) AssocLeft

prefixOp op unop = Prefix (m_reservedOp op >> return (Uno unop))

definedOp :: [MacroDef] -> Parser String
definedOp md = do m_reservedOp "defined" 
                  mac <- (m_parens m_identifier <|> m_identifier)
                  return $ if isDefined md mac then "1" else "0"

evalDefined :: [MacroDef] -> String -> String
evalDefined md s = case parse evalDefined' "" s of
                     Left e -> error "evalDefined: parse failed"
                     Right r -> r
  where
    evalDefined' = replaceAll (definedOp md) (anythingBut (alphaNum <|> char '_') >>= \c -> return $ [c])

evalExpr :: String -> Int
evalExpr inp = case parse exprparser "" inp of
                   Left _ -> error ("Failed to parse expression: " ++ inp)
                   Right e -> evalExpr' e

evalExpr' :: Expr -> Int
evalExpr' (Constant n) = n
evalExpr' (Uno Pos e) = eval_prefix e id
evalExpr' (Uno Neg e) = eval_prefix e (* (-1))
evalExpr' (Uno Not e) = eval_prefix e (\n -> if n == 0 then 1 else 0)
evalExpr' (Uno BitNot e) = eval_prefix e complement
evalExpr' (Duo Times e1 e2) = eval_infix e1 e2 (*)
evalExpr' (Duo Divide e1 e2) = eval_infix e1 e2 quot
evalExpr' (Duo Mod e1 e2) = eval_infix e1 e2 mod
evalExpr' (Duo Plus e1 e2) = eval_infix e1 e2 (+)
evalExpr' (Duo Minus e1 e2) = eval_infix e1 e2 (-)
evalExpr' (Duo ShiftL e1 e2) = eval_infix e1 e2 shiftL
evalExpr' (Duo ShiftR e1 e2) = eval_infix e1 e2 shiftR
evalExpr' (Duo Less e1 e2) = eval_infix e1 e2 (\n1 n2 -> boolToInt $ n1 < n2)
evalExpr' (Duo Greater e1 e2) = eval_infix e1 e2 (\n1 n2 -> boolToInt $ n1 > n2)
evalExpr' (Duo LTEQ e1 e2) = eval_infix e1 e2 (\n1 n2 -> boolToInt $ n1 <= n2)
evalExpr' (Duo GTEQ e1 e2) = eval_infix e1 e2 (\n1 n2 -> boolToInt $ n1 >= n2)
evalExpr' (Duo Equal e1 e2) = eval_infix e1 e2 (\n1 n2 -> boolToInt $ n1 == n2)
evalExpr' (Duo NotEqual e1 e2) = eval_infix e1 e2 (\n1 n2 -> boolToInt $ n1 /= n2)
evalExpr' (Duo BitAnd e1 e2) = eval_infix e1 e2 (.&.)
evalExpr' (Duo BitXor e1 e2) = eval_infix e1 e2 xor
evalExpr' (Duo BitOr e1 e2) = eval_infix e1 e2 (.|.)
evalExpr' (Duo And e1 e2) = eval_infix e1 e2 (\n1 n2 -> if (n1 /= 0) && (n2 /= 0) then 1 else 0)
evalExpr' (Duo Or e1 e2) = eval_infix e1 e2 (\n1 n2 -> if (n1 /= 0) || (n2 /= 0) then 1 else 0)

eval_prefix :: Expr -> (Int -> Int) -> Int
eval_prefix e op = op $ evalExpr' e

eval_infix :: Expr -> Expr -> (Int -> Int -> Int) -> Int
eval_infix e1 e2 op = (evalExpr' e1) `op` (evalExpr' e2)

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0
