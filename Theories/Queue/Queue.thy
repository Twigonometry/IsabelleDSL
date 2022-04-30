theory Queue
  imports Main StringUtils
begin

datatype 'a queue = AQueue "'a list" "'a list"

definition empty :: "'a queue" where
"empty = AQueue [] []"

fun enqueue :: "'a \<Rightarrow> 'a queue \<Rightarrow> 'a queue" where
"enqueue x (AQueue xs ys) = AQueue (x # xs) ys"

fun dequeue :: "'a queue \<Rightarrow> 'a option \<times> 'a queue" where
"dequeue (AQueue [] []) = (None, AQueue [] [])"
| "dequeue (AQueue xs (y # ys)) = (Some y, AQueue xs ys)"
| "dequeue (AQueue xs []) =
(case rev xs of y # ys \<Rightarrow> (Some y, AQueue [] ys))"

fun items :: "'a queue \<Rightarrow> 'a list" where
"items (AQueue xs ys) = xs @ ys"

datatype 'a queue = Init "'a list" "'a list" |
  Items "'a session" | Enqueue 'a "' a session" | Dequeue "'a session"

end