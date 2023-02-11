# Haskell Compiler 

This repository contains a Haskell compiler for a simple programming language. The source language consists of variable assignment, conditional if/else blocks, while loops, Seqn[Prog] to create a sequential list of instructions, and an App keyword for multiplication, division, addition, and subtraction. 

data Prog<br>
= Assign String Expr<br>
| If Expr Prog Prog<br>
| While Expr Prog<br>
| Seqn [ Prog ]<br>
  deriving (Show ,Eq)<br>

data Expr<br>
= Val Int<br>
| Var String<br>
| App Op Expr Expr<br>
deriving (Show , Eq)<br>
data Op = Add | Sub | Mul | Div deriving (Show , Eq)

This simple program calculates the sum of the integers five through ten.

<pre>
Seqn
   [ Assign " result " ( Val 5),
   Assign "i" ( Val 5),
   While
     ( App Sub ( Var "i") ( Val 10) )
     ( Seqn
      [ Assign "i" ( App Add ( Var "i") ( Val 1) ),
        Assign " result " ( App Add ( Var " result ") ( Var "i") )
      ]
    )
]
</pre>


<pre>
hello 
   this
        is a 
            example


</pre>



The compiler takes programs as input converting it to instructions. Our instructions include push, pushv, pop, do, jump, jumpz, and labels. The virtual machine that will run the instructions is stack based, so push pushes an integer to the top of the stack, pushv pushes the value of a variable to the top of the stack, pop removes the value at the top of the stack and updates the memory with that value, do uses the two values at the top of the stack and applies some operator and the subsequent result is placed on top of the stack, jump is an unconditional jump to some label, jumpz is a conditional jump to some label if the value on top of the stack is zero, and labels indicate a spot in the code. Whew.




type Stack = [ Int ]<br>
type Mem = [( String , Int ) ]<br>
type Code = [ Inst ]<br>
data Inst<br>
= PUSH Int<br>
| PUSHV String<br>

| POP String<br>
| DO Op<br>
| JUMP Label<br>
| JUMPZ Label<br>
| LABEL Label<br>
deriving (Show , Eq)<br>
type Label = Int<br>

Using our simple program from above, we compile into these instructions.

[ PUSH 5 , POP " result ",<br>
  PUSH 5 , POP "i",<br>
  LABEL 0 ,<br>
  PUSHV "i",<br>
  PUSH 10 ,<br>
  DO Sub ,<br>
  JUMPZ 1 ,<br>
  PUSHV "i",<br>
  PUSH 1 ,<br>
  DO Add ,<br>
  POP "i",<br>
  PUSHV " result ",<br>
  PUSHV "i",<br>
  DO Add ,<br>
  POP " result ",<br>
  JUMP 0 ,<br>
  LABEL 1<br>
]

[ PUSH 5, POP "result,
  PUSH 5, POP "i"








