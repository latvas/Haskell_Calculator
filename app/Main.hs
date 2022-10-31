module Main where
import Data.Char (isSpace, digitToInt)
import GHC.Read (readNumber)
import GHC.Unicode (isDigit)

data StackElements =  Num Int | Add | Sub | Mul | Div | Lbr | Rbr
    deriving (Show, Eq)


main :: IO ()
main = do 
    putStrLn "Write expression to calculate:"
    --exprString <- getLine
    
    let exprString = "11 + 2"
    let trimString = trim exprString
    putStrLn $ "Calculating: " ++ trimString
    evalExpr trimString


trim :: String -> String
trim "" = ""
trim (c:chars) 
    | isSpace c = trim chars 
    | otherwise = c: trim chars


--evalExpr :: String -> Double
evalExpr inputString = do
    let listOfElems = parseString inputString
    print listOfElems


parseString :: String -> [StackElements]
parseString "" = []
parseString (c:chars)
    | c == '+' = Add : parseString chars
    | c == '-' = Sub : parseString chars
    | c == '*' = Mul : parseString chars
    | c == '/' = Div : parseString chars
    | c == '(' = Lbr : parseString chars
    | c == ')' = Rbr : parseString chars
    | isDigit c = readNum c chars
    | otherwise = error $ "Can't parse symbol: " ++ [c]

    
readNum :: Char -> String -> [StackElements]
readNum c chars =
    Num (read (c : digs)) : parseString cs'
    where 
        (digs, cs') = span isDigit chars     --split string into 2 strings (a -> Bool) -> [a] -> ([a],[a])
        
    