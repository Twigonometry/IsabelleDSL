theory Intensional_Impossible imports Main
begin

(* Assume we could write in Isabelle a pretty-printer
that prints as a string, say, the code of functions from nat to nat: *)
consts prettyPrint :: "(nat \<Rightarrow> nat) \<Rightarrow> string" 

(* The we would have something line: *)

lemma one: "prettyPrint (\<lambda>x. x) = ''Lambda x. x''" sorry
lemma two: "prettyPrint (\<lambda>x. x + x - x) = ''Lambda x. x + x - x''" sorry

(* This immediately leads to a contradiction: *)
lemma False
proof-
  have "(\<lambda>x::nat. x) = (\<lambda>x. x + x - x)" unfolding fun_eq_iff by auto
  hence "prettyPrint (\<lambda>x::nat. x) = prettyPrint (\<lambda>x. x + x - x)" by simp
  hence "''Lambda x. x'' = ''Lambda x. x + x - x''" unfolding one two .
  (* and since thse two strings (lists of characters) are actually not equal, we obtain: *)
  thus False by auto
qed

(* Conclusion: From within a functional programming language, it is unsound to 
delve into the actual code (i.e., the intensional part) of a function. All 
we can explore is its behavior (i.e., the extensional part). From the internal 
point of view of Isabelle (as well as Haskell, OCaml, etc.) the two functions 
"\<lambda>x::nat. x" and "\<lambda>x. x + x - x" are the same item. 
*) 
end 