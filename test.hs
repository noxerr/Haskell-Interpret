data Command a = Assign String (NumExpr a) | Input String | Print String| Seq [Command a]
    | Cond (BoolExpr a) (Command a) (Command a)| Loop (BoolExpr a) (Command a)
    | NoCommand deriving(Show)

data NumExpr a = Var String | Const a | Plus (NumExpr a) (NumExpr a) | Minus (NumExpr a) (NumExpr a)
    | Times (NumExpr a) (NumExpr a) | Div (NumExpr a) (NumExpr a) deriving(Show)

data BoolExpr a = AND (BoolExpr a) (BoolExpr a) | OR (BoolExpr a) (BoolExpr a) | NOT (BoolExpr a)
    | Gt (NumExpr a) (NumExpr a) | Eq (NumExpr a) (NumExpr a)| Single Bool deriving(Show)

    
readCommand:: (Read a,Num a) => String -> Command a
readCommand entrada = Seq (readCommandList entrada)  

readCommand2:: (Read a,Num a) => String -> Command a
readCommand2 entrada
    | tokens == [] = NoCommand
    | (head tokens) == "INPUT" = Input $ tokens!!1
    | (head tokens) == "PRINT" = Print $ tokens!!1
    | (tokens!!1) == ":=" = Assign (head tokens) (readNumExpr (drop 2 tokens))
    | (head tokens) == "WHILE" = Loop (readBoolExpr (tail tokens)) (readCommand2 (unwords(drop 1 (dropWhile (/="DO") tokens)))) 
    | (head tokens) == "IF" = Cond (readBoolExpr (drop 1 (takeWhile (/="THEN") tokens))) 
            (readCommand2 (unwords(drop 1(dropWhile (/="THEN") tokens)))) 
            (readCommand2 (gotoElse (drop 1(dropWhile (/="THEN") tokens)) 1)) 
    | otherwise = Input (unwords ["Hola OTHERWISE"])
        where
            tokens = words entrada
            removeEnd palabra = (takeWhile (/= ';') palabra) == palabra

            
gotoElse:: [String] -> Int -> String
gotoElse [] _ = ""
gotoElse tokens 0 = unwords tokens
gotoElse tokens count
    | (head tokens) == "ELSE" && (count == 1) = unwords (drop 1 tokens)
    | (head tokens) == "ELSE" = gotoElse (dropWhile (finishMark) (drop 1 tokens)) (count-1)
    | (head tokens) == "IF" = gotoElse (dropWhile (finishMark) (drop 1 tokens)) (count+1)
    | otherwise = gotoElse (drop 1 tokens) count
    
      where
      finishMark a = (a /= "IF") && (a /= "ELSE")
     


gotoEnd:: [String] -> Int -> String
gotoEnd [] _ = ""
gotoEnd tokens 0 = unwords tokens
gotoEnd tokens count
    | (head tokens) == "END" && (count == 1) = unwords (drop 1 tokens)
    | (head tokens) == "END" = gotoEnd (dropWhile (finishMark) (drop 1 tokens)) (count-1)
    | (head tokens) == "IF" = gotoEnd (dropWhile (finishMark) (drop 1 tokens)) (count+1)
    | (head tokens) == "DO" = gotoEnd (dropWhile (finishMark) (drop 1 tokens)) (count+1)
    | otherwise = gotoEnd (drop 1 tokens) count
      where
        finishMark a = (a /= "IF") && (a /= "DO")

--IF X > 0 OR X = 0 OR NOT 0 > Y THEN
--WHILE X > Y
--DO
--X := X - 1;
--Z := Z * Z
--END
readCommandList:: (Read a,Num a) => String -> [Command a]
readCommandList entrada
    | tokens == [] = []
    | ((head tokens) == "IF") = [readCommand2 entrada]++(readCommandList (gotoEnd (drop 1 tokens) 1))
    | otherwise = [readCommand2 entrada]++(readCommandList (unwords(drop 1(dropWhile (checkValue) tokens))))
        where
            tokens = words entrada
            checkValue a = ((a /= "THEN") && (a /= "END") && ((last a) /= ';'))


readNumExpr::(Read a,Num a) => [String] -> NumExpr a
readNumExpr tokens
    | ((length tokens) == 1 && isNum) || (operador && isNum) = Const (read $ (removeEnd(head tokens)))
    | ((length tokens) == 1) || (operador) = Var (removeEnd(head tokens))
    | (tokens!!1) == "+" = Plus (readNumExpr ([head tokens])) (readNumExpr ([(tokens!!2)]))
    | (tokens!!1) == "-" = Minus (readNumExpr ([head tokens])) (readNumExpr ([(tokens!!2)]))
    | (tokens!!1) == "*" = Times (readNumExpr ([head tokens])) (readNumExpr ([(tokens!!2)]))
    | (tokens!!1) == "/" = Div (readNumExpr ([head tokens])) (readNumExpr ([(tokens!!2)]))
        where
            removeEnd palabra = (takeWhile (/= ';') palabra)
            isNum = (head (head tokens)) >= '0' && (head (head tokens)) <='9'
            operador 
                |(length tokens > 1) = ((tokens!!1) /= "+") && ((tokens!!1) /= "-") && ((tokens!!1) /= "*") && ((tokens!!1) /= "/")
                | otherwise = True


readBoolExpr::(Read a,Num a) => [String] -> BoolExpr a
readBoolExpr tokens --NO HE DE PASARLE THENS NI IFS
    | tokens == [] = Single True
    | ((head tokens) == "END") || ((head tokens) == "THEN") || ((head tokens) == "DO")= Single True
    | ((length tokens) == 3) && (tokens!!1) == ">" 
        = Gt (readNumExpr tokens) (readNumExpr (drop 2 tokens))
    | ((length tokens) == 3) && (tokens!!1) == "=" 
        = Eq (readNumExpr tokens) (readNumExpr (drop 2 tokens))
    
    | (head tokens) == "NOT" = NOT (readBoolExpr (tail tokens))
   
    | (tokens!!3) == "AND" = OR (readBoolExpr (takeWhile (/= "OR") tokens)) (readBoolExpr (drop 1(dropWhile (/= "OR") tokens)))
    | (tokens!!3) == "OR" = OR (readBoolExpr (takeWhile (/= "OR") tokens)) (readBoolExpr (drop 1(dropWhile (/= "OR") tokens)))
    | otherwise = Single False
    




class SymTable m where
    update:: m a -> String -> a -> m a
    value:: m a -> String -> a
    exists:: m a -> String -> Bool
    start:: m a -> m a

data LlistaParells a = Lista [(String,a)] deriving(Show)
data Tree a = Node (String,a) (Tree a) (Tree a) | Empty deriving (Show)

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

    value lst@(Lista (x:xs)) key
      | (fst x) == key = snd x
      | otherwise = value (Lista xs) key

    start m = Lista []


instance SymTable Tree where
    start m = Empty
    --start::Tree Int
    value (Node n iz der) key
      | (fst n) == key = snd n
      | (fst n) < key = value der key
      | (fst n) > key = value iz key
      
    exists Empty key = False;
    exists arb@(Node n iz der) key
      | (fst n) == key = True
      | (fst n) < key = exists der key
      | (fst n) > key = exists iz key
    
    update Empty key nValue = (Node (key, nValue) Empty Empty)
    update arb@(Node n iz der) key nValue
      | (fst n) < key = (Node n iz (update der key nValue))
      | (fst n) > key = (Node n (update iz key nValue) der)
      | (fst n) == key = (Node (key, nValue) iz der)
      
    
      



--myFilter :: (a -> Bool) -> [a] -> [a]
--myFilter f l = [x | x <- l, f x]

--myAll :: (a -> Bool) -> [a] -> Bool
--myAll f l = foldr (\x acc -> (f x) && acc) True l


--myAny :: (a -> Bool) -> [a] -> Bool
--myAny f l = foldr (\x acc -> (f x) || acc) False l











 
