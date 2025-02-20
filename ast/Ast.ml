type tree = Leaf | Node of int * value * tree * tree
and value = 
  | NumberVal of int 
  | BoolVal of bool
  | StringVal of string
  | Procedure of string list * aexpr * pexpr * (int * (string * value) list) ref
  | StructVal of (int * (string * value) list)
  | StructTemplate of string list
  | TableVal of tree
  | Null
and aexpr =
  | Var of string
  | Const of value
  | GetTableVal of string * aexpr
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
  | MakeStruct of string * aexpr list
  | GetStructField of string * aexpr
and pexpr = 
  | AssignExpr of string * aexpr
  | SubstExpr of string * aexpr
  | IfExpr of aexpr * pexpr * pexpr
  | WhileExpr of aexpr * pexpr
  | ForExpr of string * aexpr * aexpr * pexpr * pexpr
  | LoopExpr of aexpr * pexpr * pexpr
  | DeclareProcExpr of string * string list * aexpr * pexpr
  | DeclareStructExpr of string * string list
  | DeclareTableExpr of string * aexpr * aexpr
  | SubstTableExpr of string * aexpr * aexpr
  | SubstStructExpr of aexpr * aexpr
  | PrintExpr of aexpr list
  | PutEndlExpr
  | PutSpaceExpr
  | ReadIntExpr of string
  | ReadStringExpr of string
  | FreeVarExpr of string
  | FreeAllExpr
  | Comb of pexpr * pexpr
  | Skip;;
