# Haskell Compiler 

This repository contains a Haskell compiler for a simple programming language. The source language consists of variable assignment, conditional if/else blocks, while loops, Seqn[Prog] to create a sequential list of instructions, and an App keyword for multiplication, division, addition, and subtraction. 

<pre>
data Prog
= Assign String Expr
| If Expr Prog Prog
| While Expr Prog
| Seqn [ Prog ]
  deriving (Show, Eq)

data Expr
= Val Int
| Var String
| App Op Expr Expr
deriving (Show, Eq)
data Op = Add | Sub | Mul | Div deriving (Show, Eq)
</pre>

This simple program calculates the sum of the integers five through ten.

<pre>
Seqn
   [ Assign "result" ( Val 5),
   Assign "i" ( Val 5),
   While
     ( App Sub ( Var "i") ( Val 10) )
     ( Seqn
      [ Assign "i" ( App Add ( Var "i") ( Val 1) ),
        Assign "result" ( App Add ( Var "result") ( Var "i") )
      ]
    )
]
</pre>

The compiler takes programs as input converting it to instructions. Our instructions include push, pushv, pop, do, jump, jumpz, and labels. The virtual machine that will run the instructions is stack based, so push pushes an integer to the top of the stack, pushv pushes the value of a variable to the top of the stack, pop removes the value at the top of the stack and updates the memory with that value, do uses the two values at the top of the stack and applies some operator and the subsequent result is placed on top of the stack, jump is an unconditional jump to some label, jumpz is a conditional jump to some label if the value on top of the stack is zero, and labels indicate a spot in the code. Whew.

<pre>
type Stack = [ Int ]
type Mem = [( String , Int ) ]
type Code = [ Inst ]
data Inst
= PUSH Int
| PUSHV String

| POP String
| DO Op
| JUMP Label
| JUMPZ Label
| LABEL Label
deriving (Show , Eq)
type Label = Int
</pre>

Using our simple program from above, we compile into these instructions.

<pre>
[ PUSH 5 , POP " result ",
  PUSH 5 , POP "i",
  LABEL 0,
  PUSHV "i",
  PUSH 10,
  DO Sub,
  JUMPZ 1,
  PUSHV "i",
  PUSH 1,
  DO Add,
  POP "i",
  PUSHV "result",
  PUSHV "i",
  DO Add,
  POP "result",
  JUMP 0,
  LABEL 1
]
</pre>

The memory storage is type Mem = [(String, Int)], so when this program terminates we should have [("result", 45), ("i", 10)]


We run a program of the initial source language through the compiler function. This outputs program in instructions. Now, the simulated virtual machine will take the instructions and perform the computations. 

<pre>
data MachST = MachST { stack :: Stack , mem :: Mem , counter :: Int }
  deriving ( Show )

data MachST = MachST Stack Mem Int deriving ( Show )
stack :: MachST -> Stack
stack ( MachST s _ _ ) = s
mem :: MachST -> Mem
mem ( MachST _ m _ ) = m
counter :: MachST -> Int
counter ( MachST _ _ c ) = c

</pre>

MachST, or machine state, contains the state as the instructions execute. An initial state would have nothing on the stack, no memory, and a program counter at 0. 

This project contains two exec functions. One is passes the state manually and the other in monadic. 

<pre>
exec ’ :: Code -> MachST -> ( Mem , MachST )

exec :: Code -> Mem
</pre>

In the code, we see how much easier the state monad makes our lives. Running a program through our monadic virtual machine looks like exec (comp (programName inputValue))









