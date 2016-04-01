data NumericExpr e = Var e | Const e | Plus [e] | Minus [e] | Times [e] | Div e e deriving (Show,Read)
data BooleanExpr b = AND (BooleanExpr b)| OR (BooleanExpr b)|
    NOT (BooleanExpr b) | Single (BooleanExpr b) |
    Gt (NumericExpr b) (NumericExpr b) | Eq (NumericExpr b) (NumericExpr b) | Str String | Bol Bool deriving (Show,Read)

data Expr e = Num (NumericExpr e) | String (BooleanExpr e) deriving (Show, Read)

data Command a = Assign String (NumericExpr a) | Input a |Print a | Seq [Command a] |
    Cond (BooleanExpr a) | Loop (Expr a) [Command a] deriving (Show,Read)




readCommand:: Num a => String -> Command a
readCommand entrada
    | take 1 (words entrada) == ["WHILE"] = Loop (readBooleanExpr (takeWhile(/= "DO") (drop 1 (words entrada)))) []
   -- | take 1 (words entrada) == ["IF"] = Loop (readBooleanExpr (takeWhile(/= "THEN") (drop 1 (words entrada)))) []

   
   
readBooleanExpr:: Num b => [String] -> Expr b
readBooleanExpr l 
    | head(drop 1 l) == ">" = String (Str (unwords["Hola"])) --(Gt (Num(Var 3)) (Num(Var 2))) 

--readBooleanExpr2:: Num b => [String] -> Expr b
--readBooleanExpr2 entrada 
--    | take 1 entrada == ["Hola"] = Num $ Var 3
   -- | otherwise = String (Str (unwords["Hola"]))


