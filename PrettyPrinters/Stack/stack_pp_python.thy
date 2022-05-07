theory stack_pp_python
imports "../../Theories/Stack/Stack" "../../Resources/StringUtils"
begin

fun pp :: "('a::printable) session => String.literal" where
"pp (Items ses) = pp ses + STR ''.items()''" |
"pp (Push i ses) = pp ses + STR ''.push('' + (string_of i) + STR '')''" |
"pp (Pop ses) = pp ses + STR ''.pop()''" |
"pp (Init llist rlist) = (string_of llist) + (string_of rlist)"

definition blah where [code]: "blah \<equiv> pp  
(Items (Pop (Pop (Push 4 (Pop (Push 3 (Push 2 (Push (1::int) 
(Init [] [])))))))))"

export_code pp blah in Haskell

end 