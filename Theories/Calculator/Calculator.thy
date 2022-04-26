theory Calculator
  imports Main
begin

(* state is a wrapper for an int
getInt is destructor *)

datatype state = St (getInt:int)

(* calculator functions on state *)

fun clear :: "state => state" where
"clear (St m) = St 0"

fun getResult :: "state => int" where
"getResult (St m) = m"

(* basic arithmetic function definitions
perform an action on a state *)

fun add :: "state => int => state" where
"add (St m) n = St (m + n)"

fun sub :: "state => int => state" where
"sub (St m) n = St (m - n)"

fun mul :: "state => int => state" where
"mul (St m) n = St (m * n)"

fun divi :: "state => int => state" where
"divi (St m) n = St (m div n)"

(* 
model a 'session' (series of commands in python program)
e.g.
calculator c = new Calculator(); (this will be boilerplate)
c.clear().add(2).clear().getResult();
*)

datatype session = GetResult | Clear session | Add int session | Sub int session | Mul int session | Div int session

(* todo: write evals in terms of arithmetic funcs? *)

fun eval :: "state => session => state" where
"eval s GetResult = s" |
"eval (St i) (Clear ses) = eval (St 0) ses" |
"eval (St j) (Add i ses) = eval (add (St j) i) ses" |
"eval (St j) (Sub i ses) = eval (sub (St j) i) ses" |
"eval (St j) (Mul i ses) = eval (mul (St j) i) ses" |
"eval (St j) (Div i ses) = eval (divi (St j) i) ses"

value "eval (St 0) (Add 5 (Sub 4 (Div 4 (GetResult))))"

end