type value = 
  | NumberVal of int 
  | BoolVal of bool;;

type aexpr =
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
  | Not of aexpr;;

type env = (string * value) list;;

type pexpr = 
  | AssignExpr of string * aexpr
  | SubstExpr of string * aexpr
  | IfExpr of aexpr * pexpr * pexpr
  | WhileExpr of aexpr * pexpr
  | Comb of pexpr * pexpr
  | Skip;;

(* let prog = Comb (BindExpr ("x", Const (NumberVal 10)), IfExpr (EQ (Var "x", Const (NumberVal 10)), BindExpr (
  "x", Const (NumberVal 20)), BindExpr ("x", Const (BoolVal true))));; *)