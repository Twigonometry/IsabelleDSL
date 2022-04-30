theory Rbac
imports Main
begin

datatype userID = UserID string
datatype paperID = PaperID string
datatype confID = ConfID string

definition "emptyStr = STR ''''"

(* The users of the system: *)
datatype user = User String.literal String.literal String.literal
fun nameUser where "nameUser (User name info email) = name"
fun infoUser where "infoUser (User name info email) = info"
fun emailUser where "emailUser (User name info email) = email"
definition "emptyUser \<equiv> User emptyStr emptyStr emptyStr"

datatype password = Password String.literal
definition "emptyPass \<equiv> Password emptyStr"

(* Roles: author, reviewer (owner of the nth review of a paper), program committee (PC) member, chair *)
datatype role = Aut paperID | Rev paperID nat | PC | Chair

record state =
  userIDs :: "userID list"
  pass :: "userID \<Rightarrow> password"
  user :: "userID \<Rightarrow> user"
  roles :: "confID \<Rightarrow> userID \<Rightarrow> role list"

(* Create new user (user) in the system: *)
(* if given user ID already taken, generate a fresh one *)
definition createUser ::  "state \<Rightarrow> userID \<Rightarrow> password \<Rightarrow> String.literal \<Rightarrow> String.literal \<Rightarrow> String.literal \<Rightarrow> state"
where
"createUser s uID p name info email \<equiv>
 let uIDs = userIDs s
 in
 s \<lparr>userIDs := uID # uIDs,
    user := (user s) (uID := User name info email),
    pass := (pass s) (uID := p)\<rparr>"