import Control.Monad.State.Lazy 
import Data.Maybe 
import Data.List
import System.IO

-- Operations
data Op = Add | Sub | Mul | Div deriving (Show , Eq)

-- Expression
data Expr = Val Int
          | Var String
          | App Op Expr Expr deriving (Show , Eq)

-- Program
data Prog =  Assign String Expr
           | If Expr Prog Prog
           | While Expr Prog
           | Seqn [Prog] deriving (Show ,Eq)


-- Stack based virtual machine 
type Stack = [Int]
type Mem = [(String , Int)]

-- Code is list of instructions 
type Code = [Inst]

-- Instructions
data Inst = PUSH Int
          | PUSHV String 
          | POP String 
          | DO Op 
          | JUMP Label 
          | JUMPZ Label 
          | LABEL Label deriving (Show, Eq)

-- Labels 
type Label = Int 

-- Example Program
sumToTen :: Int -> Prog
sumToTen x =
    Seqn
        [ Assign "result" (Val x),
            Assign "i" (Val x),
            While
                (App Sub (Var "i") (Val 10))
                ( Seqn
                    [ Assign "i" (App Add (Var "i") (Val 1)),
                    Assign "result" (App Add (Var "result") (Var "i"))
            ]
        )
    ]

-- Hard code 5
sttCode :: Code 
sttCode =      [PUSH 5, POP "result",
                PUSH 5, POP "i",
                LABEL 1,
                PUSHV "i",
                PUSH 10,
                DO Sub ,
                JUMPZ 2,
                PUSHV "i",
                PUSH 1,
                DO Add ,
                POP "i",
                PUSHV "result",
                PUSHV "i",
                DO Add ,
                POP "result",
                JUMP 1,
                LABEL 2]


fact :: Int -> Prog 
fact n = 
    Seqn [
        Assign "fact" (Val 1),
        Assign "dec" (Val n),
        While (Var "dec") (
            Seqn [
                Assign "fact" (App Mul (Var "dec") (Var "fact")),
                Assign "dec"  (App Sub (Val 1) (Var "dec" ) )
            ]
        )
    ]

codeFact :: Int -> Code 
codeFact n = [
    PUSH 1, POP "fact",
    PUSH n, POP "dec",
    LABEL 1,
    PUSHV "dec",
    JUMPZ 2,
    PUSHV "dec",
    PUSHV "fact",
    DO Mul,
    POP "fact",
    PUSH 1,
    PUSHV "dec",
    DO Sub,
    POP "dec",
    JUMP 1,
    LABEL 2
    ]

-- Return program
addT :: Int -> Int -> Prog 
addT x y = Assign "result"(App Add(Val x) (Val y))

-- Compiler Expression
compExpr :: Expr -> Code
compExpr (Var x) = [PUSHV x]
compExpr (Val x) = [PUSH x]
compExpr (App op x y) = compExpr x ++ compExpr y ++ [DO op]

-- Manual Compiler
comp' :: Prog -> Int -> (Code, Int)
comp' (Assign v e) l = (compExpr e ++ [POP v], l)
comp' (If ex p1 p2) l = let
    (pr1, l') = comp' p1 l
    (pr2, l'') = comp' p2 l'
    in (compExpr ex ++ [JUMPZ l''] ++ pr1 ++ [JUMP(l'' + 1), LABEL l''] ++ fst (comp' p2 l) ++ [LABEL (l'' + 1)], l + 2)       -- Check vs. finish?

comp' (While ex pr) l = let
    (c,l') = comp' pr l 
    in ([LABEL l'] ++ compExpr ex ++ [JUMPZ (l' + 1)] ++ c ++ [JUMP l'] ++ [LABEL (l'+1)], l + 2)

comp' (Seqn []) l = ([], l)
comp' (Seqn xs) l = let 
    (pr1, l')  = comp' (head xs) l 
    (pr2, l'') = comp' (Seqn $ tail xs) l' 
    in (pr1 ++ pr2, l'')

-- Monadic Compiler
compM :: Prog -> State Int Code 
compM (Assign v e) = return 
    ( compExpr e ++ [POP v] )

compM (If ex p1 p2) = do
    code1 <- compM p1 
    code2 <- compM p2
    l1 <- fresh
    l2 <- fresh
    return $ compExpr ex ++ [JUMPZ l1] ++ code1 ++ [JUMP l2, LABEL l1] ++ code2 ++ [LABEL l2]

compM (While ex pr) = do 
    c <- compM pr
    l1 <- fresh
    l2 <- fresh
    return $ [LABEL l1] ++ compExpr ex ++ [JUMPZ l2] ++ c ++ [JUMP l1] ++ [LABEL l2]

compM (Seqn xs) = fmap concat $ mapM compM xs

fresh :: State Int Int
fresh = do
    l <- get
    put (l + 1) 
    return (l + 1)

-- Call Monadic Compiler
comp :: Prog -> Code
comp pr = evalState (compM (pr)) 0

-- Virtual Machine
data MachST = MachST { stack :: Stack , mem :: Mem , counter :: Int } deriving (Show)

initMachST :: MachST
initMachST = MachST { stack = [], mem = [], counter = 0 }

-- Manual Virutal Machine 
exec' :: Code -> MachST -> (Mem, MachST)
exec' code mST = if counter mST >= length code || counter mST < 0 then (mem mST, mST) else case code !! counter mST of 
    LABEL l -> let (mem', mST') = exec' code (mST { counter = counter mST + 1}) in (mem', mST')  
    PUSH n -> exec' code (inc (push n mST))
    PUSHV var -> case lookup var (mem mST) of
        Just val -> exec' code (inc (push val mST))
        Nothing -> error ("Variable not found. " ++ var)
    POP var -> exec' code (inc mST { stack = tail (stack mST), mem = (var, head (stack mST)) : mem mST})
    JUMP l -> exec' code mST {counter = labelIndex code l}
    JUMPZ l -> if (stack mST !! 0 == 0) then exec' code mST {counter = labelIndex code l} else exec' code (inc mST)
    DO op -> exec' code (inc mST {stack = opToOp op (stack mST !! 0) (stack mST !! 1) : drop 2 (stack mST)})

inc :: MachST -> MachST
inc (MachST s m c) = MachST s m (c+1)

push :: Int -> MachST -> MachST
push n mST = mST { stack = n : stack mST }

labelIndex :: Code -> Int -> Int
labelIndex code label = fromJust(elemIndex(LABEL label) code)

-- Repl Correct Operation 
opToOp :: Op -> (Int -> Int -> Int)
opToOp Add = (+)
opToOp Sub = (-)
opToOp Mul = (*)
opToOp Div = div

-- Call Monadic Compiler -- 
exec :: Code -> Mem
exec c = evalState (execM c) initMachST

-- Monadic Virtual Machine
execM :: Code -> State MachST Mem 
execM code = do
    let len = length code
    st <- get 
    if counter st >= len then return (mem st) else case code !! counter st of 
        (PUSH n) -> do 
            pushM n 
            execM code
        --(PUSH v) -> do 
        (PUSHV v) -> do
            pushvM v 
            execM code 
        (LABEL l) -> do 
            incM 
            execM code     
        (POP v) -> do 
            popM v 
            execM code 
        (DO op) -> do 
            doM op 
            execM code 
        (JUMP l) -> do 
            let ind = labelIndex code l
            jumpM ind
            execM code 
        (JUMPZ l) -> do
            let ind = labelIndex code l 
            jumpzM ind
            execM code 

jumpzM :: Int -> State MachST () 
jumpzM l = do
    st <- get 
    t <- gets (tail . stack) 
    if head (stack st) == 0 then do 
        put (st {stack = t})
        jumpM l
        
    else do 
        put (st {stack = t})
        incM 
        
jumpM :: Int -> State MachST () 
jumpM l = do 
    st <- get 
    put (st {counter = l})

doM :: Op -> State MachST ()
doM op = do 
    st <- get
    let res = opToOp op (head (stack st)) (stack st !! 1)
    put (st {stack = drop 2 (stack st)})
    pushM res 

pushvM :: String -> State MachST () 
pushvM v = do 
    st <- get

    case lookup v (mem st) of
        Just v -> put (st { stack = v : stack st })
        Nothing -> error "No Var Found."
    incM

-- Helpers
incM :: State MachST ()
incM = modify (\st -> st { counter = counter st + 1})

pushM :: Int -> State MachST ()
pushM n = do 
    incM
    modify (push n)

popM :: String -> State MachST ()
popM str = do 
    val <- gets (head . stack)
    modify (\st -> st { mem = (str, val) : mem st})
    incM


