import Control.Monad.State.Lazy 


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




-- ? 
-- add :: Int -> Int -> Prog
-- add x y = Assign "result" (Add (Val x) (Val y))


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
    
-- comp' (Seqn []) l = ([], 2)
-- comp' (Seqn xs) l =  (fst (comp' (head xs) l) ++ (fst (comp' (Seqn (tail xs)) l)), l) 


-- (fst (comp' (head xs) l) ++ (fst (comp' (Seqn (tail xs)) l)), l)

--comp' (Seqn (p:ps)) l = fst (comp' p l) ++ fst (comp' ps l)
    --undefined --(comp' p l) ++ (co
    
    
   -- mp' (Seqn (ps) l)) 

--  (fst (comp' (head xs) l) ++ (fst (comp' (Seqn xs) l)), l)

-- Add fresh function


--  let

--     (c,l') = comp' pr l 
--     in ([LABEL l'] ++ compExpr ex ++ [JUMPZ (l' + 1)] ++ c ++ [JUMP l'] ++ [LABEL (l'+1)], l + 2)



compExpr :: Expr -> Code
compExpr (Var x) = [PUSHV x]
compExpr (Val x) = [PUSH x]
compExpr (App op x y) = compExpr x ++ compExpr y ++ [DO op]



{- ?'s
 - Types for funcM
    - funcM vs func' using same steps
 - 3a -> 3b
 - Sqn State pass
 - compM St
 ------------------

-}

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

-- Call Monadic Compiler
comp :: Prog -> Code
comp pr = evalState (compM (pr)) 0


fresh :: State Int Int
fresh = do
    l <- get
    put (l + 1) 
    return (l + 1)



-- Direct Copy
-- fresh :: Eq a => a -> ST [(a,Int)] Int
-- fresh a = S (\m -> case lookup a m of 
--     Nothing -> let i = length m in (i, (a,i): m)
--     Just i -> (i,m))


-- Problem 3.f.
data MachST = MachST { stack :: Stack , mem :: Mem , counter :: Int } deriving (Show)

initMachST :: MachST
initMachST = MachST { stack = [], mem = [], counter = 0 }


-- Problem 
exec' :: Code -> MachST -> (Mem, MachST)
exec' inCode mST = case inCode !! counter mST of 
    LABEL l -> let (memory, state) = exec' inCode (mST { counter = counter mST + 1}) in (memory, state)
    --POP var -> 
    --JUMP l -> 
    -- ...
    -- n instructions?



execM :: Code -> Mem
--execM :: Code -> State Mem (MachST Mem)
execM = undefined

-- Problem 3.h.
exec :: Code -> Mem
exec c = undefined

































-- In class monad transformers -- State/Stack
{-
import x.y.TRANS?.monad?
Debug.Trace???? For Debugging?!

runState vs evalState vs execState -- Change pair retur (me, andMe) or me or andMe
Make sure to add inital stack!


-- State transfomer -- 

Push
pushM -> Add value to stack (int -> state stack ())        () = unit (No resulting value)

Rather than unit, could make this whatever we wanted
(RETURN WHATEVER VALUE WE JUST PUSHED ON) => If we want a value from state transformer
Can put in variables, raw number, for passing into state transformer


Pop
Ex


Monad transformer
Ends with xT (Capital T)
Try combos until it works
Innermost monad 
State monad wrapping writer monad 
Can use facilities of writer monad


Writer monad: 
type Writer w 
type Write Code

type Writer String


-}

