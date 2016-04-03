data Command a = Assign String (NumExpr a) | Input String | Print String| Seq [Command a]
    | Cond [BoolExpr a] (Command a) | Loop [BoolExpr a] (Command a) deriving(Show)

data NumExpr a = Var String | Const a | Plus (NumExpr a) (NumExpr a) | Minus (NumExpr a) (NumExpr a)
    | Times (NumExpr a) (NumExpr a) | Div (NumExpr a) (NumExpr a) deriving(Show)

data BoolExpr a = AND [BoolExpr a] | OR [BoolExpr a] | NOT [BoolExpr a]
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
    | (length tokens) == 1 && isNum = Const (read $ (removeEnd(head tokens)))
    | (length tokens) == 1 = Var (head tokens)
    | (tokens!!1) == "+" = Plus (readNumExpr ([head tokens])) (readNumExpr ([(tokens!!2)]))
    | (tokens!!1) == "-" = Minus (readNumExpr ([head tokens])) (readNumExpr ([(tokens!!2)]))
    | (tokens!!1) == "*" = Times (readNumExpr ([head tokens])) (readNumExpr ([(tokens!!2)]))
    | (tokens!!1) == "/" = Div (readNumExpr ([head tokens])) (readNumExpr ([(tokens!!2)]))
        where 
            removeEnd palabra = (takeWhile (/= ';') palabra);
            isNum = (head (head tokens)) >= '0' && (head (head tokens)) <='9'
  


readBoolExpr::(Read a,Num a) => [String] -> [BoolExpr a]
readBoolExpr tokens
    | tokens == [] = []
    | ((head tokens) == "END") || ((head tokens) == "THEN") || ((head tokens) == "DO")= []
    | (head tokens) == "NOT" = [NOT (readBoolExpr (tail tokens))]
    | (tokens!!1) == ">" = [Gt (readNumExpr [(head tokens)]) (readNumExpr [(tokens!!2)])]++(readBoolExpr (drop 3 tokens))
    | (head tokens) == "AND" = [AND (readBoolExpr (tail tokens))]
    | (head tokens) == "OR" = [OR (readBoolExpr (tail tokens))]
    | (tokens!!1) == "=" = [Eq (readNumExpr [(head tokens)]) (readNumExpr [(tokens!!2)])]++(readBoolExpr (drop 3 tokens))




class SymTable m where
    update:: m a -> String -> a -> m a
    value:: m a -> String -> a
    exists:: m a -> String -> Bool
    start:: m a -> m a

data LlistaParells a = Lista [(String,a)] deriving(Show)
data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

instance SymTable LlistaParells where
    update m@(Lista mem) key nvalor
      | not (exists m key) = Lista $ [(key,nvalor)] ++ mem
      | otherwise = Lista $ reemplazar mem key nvalor
        where
            reemplazar::[(String,a)]-> String -> a -> [(String,a)]
            reemplazar (x:xs) key nvalor
                | fst x == key = [(key,nvalor)] ++ xs
                | otherwise = [x] ++ (reemplazar xs key nvalor)

            --reemplazar:: m a-> String -> a -> m a
            --reemplazar m@(Lista (x:xs)) key nvalor
              --  | fst x == key = Lista $ [(key,nvalor)] ++ xs
                -- | otherwise = Lista $ [x] ++ (reemplazar2 xs key nvalor)

    exists (Lista (x:xs)) key = (fst x == key) || exists (Lista xs) key;

    value ( Lista (x:xs)) key
       | (fst x) == key = snd x
       | otherwise = value (Lista xs) key

    start m = Lista []


instance SymTable Tree where
    start m = Empty



--myFilter :: (a -> Bool) -> [a] -> [a]
--myFilter f l = [x | x <- l, f x]

--myAll :: (a -> Bool) -> [a] -> Bool
--myAll f l = foldr (\x acc -> (f x) && acc) True l


--myAny :: (a -> Bool) -> [a] -> Bool
--myAny f l = foldr (\x acc -> (f x) || acc) False l












