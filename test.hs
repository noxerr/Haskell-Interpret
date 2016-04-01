data Command a = Assign String (NumExpr a) | Input String | Print String| Seq [Command a]
    | Cond (BoolExpr a) (Command a) | Loop (BoolExpr a) (Command a) deriving(Show)

data NumExpr a = Var String | Const a | Plus (NumExpr a) (NumExpr a) | Minus (NumExpr a) (NumExpr a)
    | Times (NumExpr a) (NumExpr a) | Div (NumExpr a) (NumExpr a) deriving(Show)

data BoolExpr a = AND (BoolExpr a) (BoolExpr a) | OR (BoolExpr a) (BoolExpr a) | NOT (BoolExpr a)
    | Gt (NumExpr a) (NumExpr a) | Eq (NumExpr a) (NumExpr a)| Single Bool deriving(Show)

readCommand:: (Read a,Num a) => String -> Command a
readCommand entrada
    | (head tokens) == "INPUT" = Input $ last tokens
    | (tokens!!1) == ":=" = Assign (head tokens) (readNumExpr (drop 2 tokens))
        where
            tokens = words entrada
            --restoExpr = unwords $ drop 2 tokens
            -- Z := 3;

readNumExpr::(Read a,Num a) => [String] -> NumExpr a
readNumExpr tokens
    | (length tokens) == 1 && isNum = Const (read $ (head tokens))
        where
        isNum = (head (head tokens)) >= '0' && (head (head tokens)) <='9'