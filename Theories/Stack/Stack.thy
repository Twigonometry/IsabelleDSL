theory Stack
  imports Main
begin

datatype 'a stack = AStack "'a list" "'a list"

definition empty :: "'a stack" where
"empty = AStack [] []"

fun push :: "'a \<Rightarrow> 'a stack \<Rightarrow> 'a stack" where
"push x (AStack xs ys) = AStack (x # xs) ys"

fun pop :: "'a stack \<Rightarrow> 'a stack" where
"pop (AStack [] []) = AStack [] []"
| "pop (AStack (x # xs) ys) = AStack xs ys"
| "pop (AStack [] xs) =
(case rev xs of y # ys \<Rightarrow> AStack ys [])"

fun items :: "'a stack \<Rightarrow> 'a list" where
"items (AStack xs ys) = xs @ ys"

datatype 'a session = Init "'a list" "'a list" |
  Items "'a session" | Push 'a "'a session" | Pop "'a session"

theorem pop_push [simp]: "items (pop (push x (empty))) = items (empty)"
  by (simp add: Stack.empty_def)

end