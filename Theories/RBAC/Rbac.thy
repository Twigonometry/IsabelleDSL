theory Rbac
imports Main
begin

datatype userID = UserID string

(* The users of the system: *)
datatype user = User string string string
fun nameUser where "nameUser (User name info email) = name"
fun infoUser where "infoUser (User name info email) = info"
fun emailUser where "emailUser (User name info email) = email"
definition "emptyUser \<equiv> User emptyStr emptyStr emptyStr"

datatype password = Password string
definition "emptyPass \<equiv> Password emptyStr"

(* Roles: author, reviewer (owner of the nth review of a paper), program committee (PC) member, chair *)
datatype role = Aut paperID | Rev paperID nat | PC | Chair

record state =
  userIDs :: "userID list"
  pass :: "userID \<Rightarrow> password"
  user :: "userID \<Rightarrow> user"
  roles :: "confID \<Rightarrow> userID \<Rightarrow> role list"