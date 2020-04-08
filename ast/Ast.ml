type value = 
  | NumberVal of int 
  | BoolVal of bool
  | Procedure of string list * aexpr * pexpr * (int * (string * value) list)
and aexpr =
  | Var of string
  | Const of value
  | Add of aexpr * aexpr
  | Sub of aexpr * aexpr
  | Mult of aexpr * aexpr
  | Div of aexpr * aexpr
  | Lt of aexpr * aexpr
  | Leq of aexpr * aexpr
  | Gt of aexpr * aexpr
  | Geq of aexpr * aexpr
  | Eq of aexpr * aexpr
  | Not of aexpr
  | EvalProc of string * aexpr list
and pexpr = 
  | AssignExpr of string * aexpr
  | SubstExpr of string * aexpr
  | IfExpr of aexpr * pexpr * pexpr
  | WhileExpr of aexpr * pexpr
  | DeclareProcExpr of string * string list * aexpr * pexpr
  | Comb of pexpr * pexpr
  | Skip;;
