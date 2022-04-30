theory StringUtils
imports "HOL-Library.Code_Target_Int" "HOL-Library.Code_Target_Nat"
begin

class printable = 
  fixes string_of :: "'a \<Rightarrow> String.literal"

(* string functions *)

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

instantiation nat :: printable
begin
  fun string_of_nat :: "nat \<Rightarrow> String.literal"
    where
      "string_of_nat n =
        (if n < 10 then string_of_digit n
        else string_of_digit (n div 10) + string_of_digit (n mod 10))"
instance apply standard .
end
declare string_of_nat.simps [simp del]


instantiation int :: printable
begin 
  fun string_of_int :: "int => String.literal"
  where
    "string_of_int i =
      (if i < 0 then STR ''-'' + string_of (nat (- i)) else string_of (nat i))"
instance apply standard .
end

value "string_of (2::int)"

instantiation list :: (printable)printable
begin 
  fun string_of_list :: "'a list \<Rightarrow> String.literal"
  where
    "string_of_list (x # xs) = (string_of x) + (string_of_list xs)" |
    "string_of_list [] = STR ''''"
instance by standard
end

end