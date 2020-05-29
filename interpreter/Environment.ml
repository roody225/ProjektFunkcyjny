
exception EnvironmentError of string;;

let raiseErr msg = raise (EnvironmentError (String.concat " " msg))

let emptyenv = (0, []);;

let rec envlookup varname env = 
  match env with 
    | (_, []) -> raiseErr ["Variable"; varname; "not in scope!"]
    | (_, (name, vval)::rest) -> if name = varname then 
                              vval 
                            else
                              envlookup varname (0, rest);;

let envadd varname vval (numel, env) = (numel+1, (varname, vval)::env);;

let rec envsubst varname newval env =
  match env with 
    | (_, []) -> raiseErr ["Variable"; varname; "not in scope!"]
    | (numel, (name, vval)::rest) -> if name = varname then 
                              (numel, (name, newval)::rest) 
                            else 
                              let (_, tmpe) = envsubst varname newval (numel, rest) in
                              (numel, (name, vval)::(tmpe));;

let rec envstructsubst vvar vval env = 
  match vvar with
    | (Ast.GetStructField (name, field)) ->
      (let act = envlookup name env in
      match act with
        | Ast.StructVal (x) -> (match field with
                            | Ast.Var (y) -> envsubst name (Ast.StructVal (envsubst y vval x)) env
                            | _ -> envsubst name (Ast.StructVal (envstructsubst field vval x)) env)
        | _ -> raiseErr ["Variable"; name; "is not a struct!"])
    | _ -> raiseErr ["Undefined behavior!"];;

let rec envcut numel (actnumel, env) = if numel = actnumel then 
                                         (numel,  env)
                                       else
                                         match env with
                                           | [] -> raiseErr ["Run Time Error!"]
                                           | (_::rest) -> envcut numel (actnumel-1, rest);; 

let rec envmaketab left right vval =
  if left >= right then Ast.Leaf
  else if left + 1 = right then Ast.Node(left, vval, Ast.Leaf, Ast.Leaf)
  else let mid = (left + right) / 2 in Ast.Node(mid, vval, envmaketab left mid vval, envmaketab (mid+1) right vval);;

let rec envgettableval tab i=
  match tab with
    | Ast.Leaf -> raiseErr ["table index out of range!"]
    | Ast.Node (id, v, left, right) -> if id = i then v
                                   else if i < id then envgettableval left i
                                   else envgettableval right i;;
                  
let rec tablesubst x i vval =
  match x with 
    | Ast.Leaf -> raiseErr ["table index out of range!"]
    | Ast.Node (id, v, left, right) -> if id = i then Ast.Node(id, vval, left, right)
                                   else if i < id then Ast.Node(id, v, tablesubst left i vval, right)
                                   else Ast.Node(id, v, left, tablesubst right i vval);;
          
let envtablesubst name i vval env =
  let tab = envlookup name env in
  match tab with
    | Ast.TableVal (x) -> envsubst name (Ast.TableVal((tablesubst x i vval))) env
    | _ -> raiseErr [name; "is not a table!"];;
                                  
let rec envdrop varname env = 
  match env with
    | (_, []) -> raiseErr ["Variable"; varname; "not in scope!"]
    | (x, (name, vval)::rest) -> if varname = name then (x-1, rest) else envadd name vval (envdrop name (x-1, rest));;