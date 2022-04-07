theory Calculator
imports Main "HOL-Library.Code_Target_Int" "HOL-Library.Code_Target_Nat"
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

fun add :: "int => state => state" where
"add m (St n) = St (m + n)"

fun sub :: "int => state => state" where
"sub m (St n) = St (m - n)"

fun mul :: "int => state => state" where
"mul m (St n) = St (m * n)"

fun divi :: "int => state => state" where
"divi m (St n) = St (m div n)"

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
"eval (St i) (Clear ses)  = eval (St 0) ses" |
"eval (St j) (Add i ses) = eval (add i (St j)) ses" |
"eval (St j) (Sub i ses) = eval (sub i (St j)) ses" |
"eval (St j) (Mul i ses) = eval (mul i (St j)) ses" |
"eval (St j) (Div i ses) = eval (divi i (St j)) ses"

(* string functions *)

(*

fun string_of_digit :: "nat => string"
  where
    "string_of_digit n =
      (if n = 0 then ''0''
      else if n = 1 then ''1''
      else if n = 2 then ''2''
      else if n = 3 then ''3''
      else if n = 4 then ''4''
      else if n = 5 then ''5''
      else if n = 6 then ''6''
      else if n = 7 then ''7''
      else if n = 8 then ''8''
      else ''9'')"

*)

fun string_of_digit :: "nat => String.literal"
where
  "string_of_digit n =
    (if n = 0 then STR ''0''
    else if n = 1 then STR ''1''
    else if n = 2 then STR ''2''
    else if n = 3 then STR ''3''
    else if n = 4 then STR ''4''
    else if n = 5 then STR ''5''
    else if n = 6 then STR ''6''
    else if n = 7 then STR ''7''
    else if n = 8 then STR ''8''
    else STR ''9'')"

fun string_of_nat :: "nat \<Rightarrow> String.literal"
  where
    "string_of_nat n =
(if n < 10 then string_of_digit n
      else string_of_nat (n div 10) + string_of_digit (n mod 10))"
  declare string_of_nat.simps [simp del]

(*
fun string_of_nat :: "nat => string"
  where
    "string_of_nat n =
      (if n < 10 then string_of_digit n
      else string_of_nat (n div 10) @ string_of_digit (n mod 10))"
  declare string_of_nat.simps [simp del]
*)

definition string_of_int :: "int => String.literal"
  where
    "string_of_int i =
      (if i < 0 then STR ''-'' + string_of_nat (nat (- i)) else string_of_nat (nat i))"

(*
definition string_of_int :: "int => string"
  where
    "string_of_int i =
      (if i < 0 then ''-'' @ string_of_nat (nat (- i)) else string_of_nat (nat i))"
*)

fun pp :: "session => String.literal" where 
"pp GetResult = STR ''.getResult()''" |
"pp (Clear ses) = STR ''.clear()'' + pp ses" |
"pp (Add i ses) = STR ''.add('' + (string_of_int i) + STR '')'' + pp ses" |
"pp (Sub i ses) = STR ''.sub('' + (string_of_int i) + STR '')'' + pp ses" |
"pp (Mul i ses) = STR ''.mul('' + (string_of_int i) + STR '')'' + pp ses" |
"pp (Div i ses) = STR ''.div('' + (string_of_int i) + STR '')'' + pp ses"

export_code pp in Haskell module_name Calculator file_prefix calculator

ML {*
val gen_files = Generated_Files.get_files (Proof_Context.theory_of @{context})
val output_dir = Path.explode "./generatedHaskellFiles/"
*}

ML {* map (Generated_Files.write_file output_dir) gen_files *}

end