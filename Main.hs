module Main where

import Data.Char (digitToInt, isSpace)
import GHC.Read (readNumber)
import GHC.Unicode (isDigit)

data Token = TokNum Float | TokOp Operator
  deriving (Show, Eq)

data Operator = Add | Sub | Mul | Div | Lbr | Rbr
  deriving (Show, Eq)

main :: IO ()
main = do
  putStrLn "Write expression to calculate:"
  -- let exprString = "46+(8*4)/2"
  exprString <- getLine
  let trimString = trim exprString
  putStrLn $ "Calculating: " ++ trimString
  let listOfElems = parseString trimString
  -- print listOfElems
  let infixList = infixToPostfix listOfElems []
  -- print infixList
  let result = evalExpr infixList []
  putStrLn $ "Result: " ++ show result

trim :: String -> String
trim "" = ""
trim (c : chars)
  | isSpace c = trim chars
  | otherwise = c : trim chars

isDigitOrDot :: Char -> Bool
isDigitOrDot a = a == '.' || isDigit a

readNum :: Char -> String -> [Token]
readNum c chars = TokNum (read (c : digs)) : parseString cs'
  where
    (digs, cs') = span isDigitOrDot chars -- split string into 2 strings (a -> Bool) -> [a] -> ([a],[a])

parseString :: String -> [Token]
parseString "" = []
parseString (c : chars)
  | c == '+' = TokOp Add : parseString chars
  | c == '-' = TokOp Sub : parseString chars
  | c == '*' = TokOp Mul : parseString chars
  | c == '/' = TokOp Div : parseString chars
  | c == '(' = TokOp Lbr : parseString chars
  | c == ')' = TokOp Rbr : parseString chars
  | isDigit c = readNum c chars
  | otherwise = error $ "Can't parse symbol: " ++ [c]

opPriority :: Operator -> Int
opPriority op
  | op == Mul = 3
  | op == Div = 3
  | op == Add = 2
  | op == Sub = 2
  | op == Lbr = 1
  | otherwise = error "Operator has no priority"

-- [TokenList] -> [OpStack] -> [PostfixList]
infixToPostfix :: [Token] -> [Operator] -> [Token]
infixToPostfix [TokNum x] [] = [TokNum x] -- Have a Number
infixToPostfix (TokNum x : tokens) opStack = TokNum x : infixToPostfix tokens opStack -- Have a Number
infixToPostfix (TokOp Lbr : tokens) opStack = infixToPostfix tokens (Lbr : opStack) -- Have a '('
infixToPostfix (TokOp Rbr : tokens) (op : opStack) -- Have a ')'
  | op == Mul = TokOp Mul : infixToPostfix (TokOp Rbr : tokens) opStack
  | op == Div = TokOp Div : infixToPostfix (TokOp Rbr : tokens) opStack
  | op == Add = TokOp Add : infixToPostfix (TokOp Rbr : tokens) opStack
  | op == Sub = TokOp Sub : infixToPostfix (TokOp Rbr : tokens) opStack
  | op == Lbr = infixToPostfix tokens opStack
infixToPostfix (TokOp op : tokens) [] = infixToPostfix tokens [op] -- Have Operator and empty OpStack
infixToPostfix (TokOp op : tokens) (opFromStack : opStack) =
  if opPriority opFromStack >= opPriority op -- Have Operator
    then TokOp opFromStack : infixToPostfix tokens (op : opStack)
    else infixToPostfix tokens (op : opFromStack : opStack)
infixToPostfix [] [op] = [TokOp op] -- Have empty TokenList
infixToPostfix [] (op : opStack) = TokOp op : infixToPostfix [] opStack -- Have empty TokenList

applyOp :: Operator -> Float -> Float -> Float
applyOp Add a b = a + b
applyOp Sub a b = a - b
applyOp Mul a b = a * b
applyOp Div a b = a / b

-- [TokenList] -> [NumStack] -> Result
evalExpr :: [Token] -> [Float] -> Float
evalExpr [] (x : _) = x
evalExpr (TokNum x : tokens) stack = evalExpr tokens (x : stack)
evalExpr (TokOp op : tokens) (x : y : xs) = evalExpr tokens (applyOp op y x : xs)