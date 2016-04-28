data Command a = Assign String (NumExpr a) | Input String | Print String| Seq [Command a]
    | Cond (BoolExpr a) (Command a) (Command a)| Loop (BoolExpr a) (Command a)
    | NoCommand 

data NumExpr a = Var String | Const a | Plus (NumExpr a) (NumExpr a) | Minus (NumExpr a) (NumExpr a)
    | Times (NumExpr a) (NumExpr a) | Div (NumExpr a) (NumExpr a) deriving(Show)

data BoolExpr a = AND (BoolExpr a) (BoolExpr a) | OR (BoolExpr a) (BoolExpr a) | NOT (BoolExpr a)
    | Gt (NumExpr a) (NumExpr a) | Eq (NumExpr a) (NumExpr a)| Single Bool deriving(Show)



instance (Show a) => Show (Command a) where
    show(Input string) = "INPUT " ++ show string ++ ";"
    show(Assign str nexpr) = show str ++ " = " ++ show nexpr ++ ";"
    show(Print str) = "PRINT " ++ show str ++ ";"
    show(Seq list) = unlines $ map show list
    show(Cond bEx cIf cElse) = "IF " ++ show bEx ++ " THEN\n  " ++ show cIf ++ "ELSE\n  " ++ show cElse ++ "END"
    show(Loop bEx loop) = "WHILE " ++ show bEx ++ "\nDO\n  " ++ show loop ++ "END"
    show(cmd) = show cmd



    
readCommand:: (Read a,Num a) => String -> Command a
readCommand entrada = Seq (readCommandList entrada)  

readCommand2:: (Read a,Num a) => String -> Command a
readCommand2 entrada
    | tokens == [] = NoCommand
    | (head tokens) == "INPUT" = Input (removeEnd (tokens!!1))
    | (head tokens) == "PRINT" = Print (removeEnd (tokens!!1))
    | (tokens!!1) == ":=" = Assign (head tokens) (readNumExpr (drop 2 tokens))
    | (head tokens) == "WHILE" = Loop (readBoolExpr (drop 1 (takeWhile (/= "DO") tokens))) (Seq (readCommandList (unwords(drop 1 (dropWhile (/="DO") tokens))))) --FIX WHILE
    | (head tokens) == "IF" = Cond (readBoolExpr (drop 1 (takeWhile (/="THEN") tokens))) 
            (Seq (readCommandList (unwords (takeUntilElse (drop 1 (dropWhile (/="THEN") tokens)) 1)) ))
            --(readCommand2 (unwords(drop 1(dropWhile (/="THEN") tokens)))) 
            --(Seq (readCommandList (gotoElse (drop 1(dropWhile (/="THEN") tokens)) 1)))
            (Seq (readCommandList (unwords (takeUntilEnd(words (gotoElse (drop 1(dropWhile (/="THEN") tokens)) 1)) 1)) ))
            --(readCommand2 (gotoElse (drop 1(dropWhile (/="THEN") tokens)) 1)) 
    | (head tokens) == "ELSE" = Input (unwords ["Hola Else"])
    | otherwise = Print (unwords ["Hola OTHERWISE", (head tokens)])
        where
            tokens = words entrada
            removeEnd palabra = (takeWhile (/= ';') palabra)

            
gotoElse:: [String] -> Int -> String
gotoElse [] _ = ""
gotoElse tokens 0 = unwords tokens
gotoElse tokens count
    | (head tokens) == "ELSE" && (count == 1) = unwords (drop 1 tokens) --goto end
    | (head tokens) == "ELSE" = gotoElse (dropWhile (finishMark) (drop 1 tokens)) (count-1)
    | (head tokens) == "IF" = gotoElse (dropWhile (finishMark) (drop 1 tokens)) (count+1)
    | otherwise = gotoElse (drop 1 tokens) count
    
      where
      finishMark a = (a /= "IF") && (a /= "ELSE")
     



takeUntilEnd:: [String] -> Int -> [String]
takeUntilEnd [] _ = []
takeUntilEnd tokens 0 = tokens
takeUntilEnd tokens count
    | (head tokens) == "END" && (count == 1) = []
    | (head tokens) == "END" = [head tokens] ++ (takeUntilEnd (tail tokens) (count-1))
    | (head tokens) == "IF" = [head tokens] ++ (takeUntilEnd (tail tokens) (count+1))
    | (head tokens) == "DO" = [head tokens] ++ (takeUntilEnd (tail tokens) (count+1))
    | otherwise = [head tokens] ++ (takeUntilEnd (tail tokens) count)



takeUntilElse:: [String] -> Int -> [String]
takeUntilElse [] _ = []
takeUntilElse tokens 0 = tokens
takeUntilElse tokens count
    | (head tokens) == "ELSE" && (count == 1) = []
    | (head tokens) == "ELSE" = [head tokens] ++ (takeUntilElse (tail tokens) (count-1))
    | (head tokens) == "IF" = [head tokens] ++ (takeUntilElse (tail tokens) (count+1))
    | otherwise = [head tokens] ++ (takeUntilElse (tail tokens) count)



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
        finishMark a = (a /= "IF") && (a /= "DO") && (a /= "END")

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
    | ((head tokens) == "WHILE") = [readCommand2 entrada]++(readCommandList (gotoEnd (drop 1 tokens) 1))
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
      
    

evalExprNum :: (Num a,SymTable m,Ord a) => m a -> NumExpr a -> a
evalExprNum mem (Const n) = n
evalExprNum mem (Var x) = value mem x
evalExprNum mem (Plus n n2) = (evalExprNum mem n) + (evalExprNum mem n2)
evalExprNum mem (Div n n2) = myDiv (evalExprNum mem n) (evalExprNum mem n2)
evalExprNum mem (Times n n2) = (evalExprNum mem n) + (evalExprNum mem n2)
evalExprNum mem (Minus n n2) = (evalExprNum mem n) - (evalExprNum mem n2)


myDiv :: (Num a, Ord a) => a -> a -> a
myDiv dividendo divisor 
  | dividendo < divisor = 0
myDiv dividendo divisor = 1+(myDiv (dividendo-divisor) divisor)

evalBoolExpr :: (Num a,SymTable m,Ord a) => m a -> BoolExpr a -> Bool
evalBoolExpr mem (Gt n1 n2) = (evalExprNum mem n1) > (evalExprNum mem n2)
evalBoolExpr mem (AND b1 b2) = (evalBoolExpr mem b1) && (evalBoolExpr mem b2)
evalBoolExpr mem (OR b1 b2) = (evalBoolExpr mem b1) || (evalBoolExpr mem b2)
evalBoolExpr mem (Eq n1 n2) = (evalExprNum mem n1) == (evalExprNum mem n2)
evalBoolExpr mem (NOT b1) = not (evalBoolExpr mem b1)
evalBoolExpr mem _ = True
    

interpretCommand :: (SymTable m, Num a, Ord a) => m a -> [a] -> Command a -> ((Either [a] String),m a, [a])
interpretCommand mem inputs (Assign varName exp1) = ((Left []), (update mem varName (evalExprNum mem exp1)) , inputs)
interpretCommand mem inputs@(x:xs) (Input string) = ((Left []), (update mem string x),xs)
interpretCommand mem inputs (Seq list) = evaluateListInstr mem inputs list
interpretCommand mem inputs (Cond bol commands comElse) 
  | evalBoolExpr mem bol = interpretCommand mem inputs commands
  | otherwise = interpretCommand mem inputs comElse
interpretCommand mem inputs (Loop bol commands) 
  | evalBoolExpr mem bol = interpretCommand mem inputs commands
  | otherwise = ((Left []), mem, inputs)
interpretCommand mem inputs (Print var) = ((Left [value mem var]), mem, inputs)


evaluateListInstr:: (SymTable m, Num a, Ord a) => m a -> [a] -> [Command a] -> ((Either [a] String),m a, [a])
evaluateListInstr mem inputs (x:[]) = interpretCommand mem inputs x
evaluateListInstr mem inputs (x:xs) = evaluateListInstr newMem ins xs --evaluar command antes que el resto de la lista
    where
        ret@((Left []), newMem , ins) = interpretCommand mem inputs x


interpretProgram:: (Num a, Ord a) => [a] -> Command a -> (Either [a] String)
interpretProgram inputs command = outputs
   where
     (outputs, mem, inputs2) = interpretCommand (start Empty) inputs command



expandAux :: [Command a] -> [Command a]
expandAux [] = []
expandAux (c1:clist) = (expand c1):(expandAux clist)


expand :: Command a -> Command a
expand comm@(Cond (OR b1 b2) commands comElse)
   = (Cond b1 commands (Cond b2 commands comElse))
expand comm@(Cond (AND b1 b2) commands comElse)
   = (Cond b1 (Cond b2 commands comElse) comElse)
expand comm@(Cond _ commands comElse) = comm
expand comm@(Seq commands) = Seq (expandAux commands)
expand comm = comm






instance (Eq a) => Eq (Command a) where
  c1 == c2 = c1 == c2
  
  --Leaf a == Leaf b = a == b
  --(Branch l1 r1) == (Branch l2 r2)  =  (l1==l2) && (r1==r2)
  --_  == _   =  False
--myFilter :: (a -> Bool) -> [a] -> [a]
--myFilter f l = [x | x <- l, f x]

--myAll :: (a -> Bool) -> [a] -> Bool
--myAll f l = foldr (\x acc -> (f x) && acc) True l


--myAny :: (a -> Bool) -> [a] -> Bool
--myAny f l = foldr (\x acc -> (f x) || acc) False l











 
