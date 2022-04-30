theory Rbac
imports Main
begin

datatype userID = UserID String.literal
datatype paperID = PaperID String.literal
datatype confID = ConfID String.literal

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

definition istate :: state
where
"istate \<equiv>
\<lparr>
userIDs = [],
pass = (\<lambda> uID. emptyPass),
user = (\<lambda> uID. emptyUser),
roles = (\<lambda> confID uID. []),
\<rparr>"

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

datatype userCreation = CreateUser state userID password String.literal String.literal String.literal userCreation | Commit

fun pp_uc :: "userCreation \<Rightarrow> String.literal" where
"pp_uc ((CreateUser s (UserID uid) (Password p) name info email) uc) = STR ''SINGLEQUOTEuserIDSINGLEQUOTE RIGHTARROW '' + uid + STR '', SINGLEQUOTEpasswordSINGLEQUOTE RIGHTARROW '' + p + STR '', SINGLEQUOTEnameSINGLEQUOTE RIGHTARROW '' + name + STR '', SINGLEQUOTEinfoSINGLEQUOTE RIGHTARROW '' + info + STR '', SINGLEQUOTEinfoSINGLEQUOTE RIGHTARROW '' + email + STR '','' + pp_uc uc" |
"pp_uc Commit = STR ''''"

value "pp_uc ((CreateUser istate (UserID STR ''1'') (Password STR ''password'') STR ''Mac'' STR ''Student'' STR ''mac@example.com'') (Commit))"

end