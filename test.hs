data Command a = Assign String (NumExpr a) | Input String | Print String| Seq [Command a]
    | Cond [BoolExpr a] (Command a) | Loop [BoolExpr a] (Command a) deriving(Show)

data NumExpr a = Var String | Const a | Plus (NumExpr a) (NumExpr a) | Minus (NumExpr a) (NumExpr a)
    | Times (NumExpr a) (NumExpr a) | Div (NumExpr a) (NumExpr a) deriving(Show)

data BoolExpr a = AND (BoolExpr a) (BoolExpr a) | OR (BoolExpr a) (BoolExpr a) | NOT (BoolExpr a)
    | Gt (NumExpr a) (NumExpr a) | Eq (NumExpr a) (NumExpr a)| Single Bool deriving(Show)

readCommand:: (Read a,Num a) => String -> Command a
readCommand entrada
    | (head tokens) == "INPUT" = Input $ tokens!!1
    | (head tokens) == "PRINT" = Print $ tokens!!1
    | (tokens!!1) == ":=" = Assign (head tokens) (readNumExpr (drop 2 tokens))
    | (head tokens) == "WHILE" = Loop (readBoolExpr (tail tokens)) (readCommand (unwords(drop 1(dropWhile (/="DO") tokens)))) 
    | (head tokens) == "IF" = Cond (readBoolExpr (tail tokens)) (readCommand (unwords(drop 1(dropWhile (/="THEN") tokens)))) 
    | (head tokens) == "DO" = Input (unwords ["Hola"])
        where
            tokens = words entrada
--IF X > 0 OR X = 0 OR NOT 0 > Y THEN
--WHILE X > Y
--DO
--X := X - 1;
--Z := Z * Z
--END
readCommandList:: (Read a,Num a) => String -> [Command a]
readCommandList entrada
    | tokens == [] = []
    | otherwise = [readCommand entrada]++(readCommandList (unwords(drop 1(dropWhile (checkValue) tokens))))
        where
            tokens = words entrada
            checkValue a = ((a /= "THEN") && (a /= "END") && ((last a) /= ';'))


readNumExpr::(Read a,Num a) => [String] -> NumExpr a
readNumExpr tokens
    | (length tokens) == 1 && isNum = Const (read $ (head tokens))
        where
        isNum = (head (head tokens)) >= '0' && (head (head tokens)) <='9'


readBoolExpr::(Read a,Num a) => [String] -> [BoolExpr a]
readBoolExpr tokens
    | ((head tokens) == "END") || ((head tokens) == "THEN") || ((head tokens) == "DO")= []
    | (head tokens) == "NOT" = [NOT (head $ readBoolExpr (tail tokens))]++(readBoolExpr (tail tokens))
    | (tokens!!1) == ">" = [Gt (readNumExpr [(head tokens)]) (readNumExpr [(tokens!!2)])]++(readBoolExpr (drop 3 tokens))



--myFilter :: (a -> Bool) -> [a] -> [a]
--myFilter f l = [x | x <- l, f x]

--myAll :: (a -> Bool) -> [a] -> Bool
--myAll f l = foldr (\x acc -> (f x) && acc) True l


--myAny :: (a -> Bool) -> [a] -> Bool
--myAny f l = foldr (\x acc -> (f x) || acc) False l












