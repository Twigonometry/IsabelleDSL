theory Stack
  imports Main StringUtils
begin

datatype 'a stack = AStack "'a list" "'a list"

definition empty :: "'a stack" where
"empty = AStack [] []"

primrec push :: "'a \<Rightarrow> 'a stack \<Rightarrow> 'a stack" where
"push x (AStack xs ys) = AStack (x # xs) ys"

fun pop :: "'a stack \<Rightarrow> 'a stack" where
"pop (AStack [] []) = AStack [] []"
| "pop (AStack (x # xs) ys) = AStack xs ys"
| "pop (AStack [] xs) =
(case rev xs of y # ys \<Rightarrow> AStack ys [])"

fun items :: "'a stack \<Rightarrow> 'a list" where
"items (AStack xs ys) = xs @ ys"

datatype session = Items | Push int session | Pop session

fun pp :: "session => String.literal" where
"pp Items = STR ''.items()''" |
"pp (Push i ses) = STR ''.push('' + (string_of_int i) + STR '')'' + pp ses" |
"pp (Pop ses) = STR ''.pop()'' + pp ses"

end