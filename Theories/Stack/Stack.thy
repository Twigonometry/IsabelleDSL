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

datatype session = Items session | Push int session | Pop session | "'a list" "'a list"

fun pp :: "session => String.literal" where
"pp (Items ses) = pp ses + STR ''.items()''" |
"pp (Push i ses) = pp ses + STR ''.push('' + (string_of_int i) + STR '')''" |
"pp (Pop ses) = pp ses + STR ''.pop()''" |
"pp llist rlist = (string_of_int_list llist) + (string_of_int_list rlist)"

end