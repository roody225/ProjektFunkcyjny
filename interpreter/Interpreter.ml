open Ast;;
open Environment;;

exception RunTimeError of string;;

let raiseErr msg = raise (RunTimeError (String.concat " " msg))

let rec mapargs argslist argsvals env =
  let (numel, vars) = env in
  match (argslist, argsvals) with
    | ([], []) -> env
    | (name::alist, vval::aval) -> mapargs alist aval (numel+1, (name, vval)::vars)
    | (_, _) -> raiseErr ["too few/many arguments in a function/struct!"];;

let rec eval expr env = 
  match expr with
    | Const c -> c
    | Var v -> envlookup v env
    | Add (l, r) -> (match (eval l env, eval r env) with
                      | (NumberVal left, NumberVal right) -> NumberVal (left + right)
                      | (StringVal left, StringVal right) -> StringVal ((String.concat "" [left; right]))
                      | _ -> raiseErr ["Bad argument in + operator!"])
    | Sub (l, r) -> (match (eval l env, eval r env) with
                      | (NumberVal left, NumberVal right) -> NumberVal (left - right)
                      | _ -> raiseErr ["Bad argument in - operator!"])
    | Div (l, r) -> (match (eval l env, eval r env) with
                      | (NumberVal left, NumberVal right) -> (if right=0 then 
                                                               raiseErr ["division by 0!"]
                                                             else 
                                                               NumberVal (left / right))
                      | _ -> raiseErr ["Bad argument in / operator!"])
    | Mult (l, r) -> (match (eval l env, eval r env) with
                      | (NumberVal left, NumberVal right) -> NumberVal (left * right)
                      | _ -> raiseErr ["Bad argument in * operator!"])
    | Lt (l, r) -> (match (eval l env, eval r env) with
                      | (NumberVal left, NumberVal right) -> BoolVal (left < right)
                      | (BoolVal left, BoolVal right) -> BoolVal (left < right)
                      | (StringVal left, StringVal right) -> BoolVal (left < right)
                      | _ -> raiseErr ["Bad argument in < operator!"])     
    | Leq (l, r) -> (match (eval l env, eval r env) with
                      | (NumberVal left, NumberVal right) -> BoolVal (left <= right)
                      | (BoolVal left, BoolVal right) -> BoolVal (left <= right)
                      | (StringVal left, StringVal right) -> BoolVal (left <= right)
                      | _ -> raiseErr ["Bad argument in <= operator!"])    
    | Gt (l, r) -> (match (eval l env, eval r env) with
                      | (NumberVal left, NumberVal right) -> BoolVal (left > right)
                      | (BoolVal left, BoolVal right) -> BoolVal (left > right)
                      | (StringVal left, StringVal right) -> BoolVal (left > right)
                      | _ -> raiseErr ["Bad argument in > operator!"])   
    | Geq (l, r) -> (match (eval l env, eval r env) with
                      | (NumberVal left, NumberVal right) -> BoolVal (left >= right)
                      | (BoolVal left, BoolVal right) -> BoolVal (left >= right)
                      | (StringVal left, StringVal right) -> BoolVal (left >= right)
                      | _ -> raiseErr ["Bad argument in >= operator!"])
    | Eq (l, r) -> (match (eval l env, eval r env) with
                      | (NumberVal left, NumberVal right) -> BoolVal (left = right)
                      | (BoolVal left, BoolVal right) -> BoolVal (left = right)
                      | (StringVal left, StringVal right) -> BoolVal (left = right)
                      | _ -> raiseErr ["Bad argument in == operator!"])
    | Not e -> (match eval e env with 
                  | BoolVal b -> BoolVal (not b)
                  | _ -> raiseErr ["Bad argiment in ! operator!"])
    | EvalProc (name, argsvals) -> (match envlookup name env with 
                                     | Procedure (argslist, returnval, body, closure) ->
                                       eval returnval (interp body (mapargs argslist (List.map (fun a -> eval a env) argsvals) !closure))
                                     | _ -> raiseErr [name; "is not a procedure!"])
    | MakeStruct (name, fieldsvals) -> (match envlookup name env with
                                         | StructTemplate (fields) ->
                                           StructVal (mapargs fields (List.map (fun a -> eval a env) fieldsvals) emptyenv)
                                         | _ -> raiseErr [name; "does not name a struct!"])
    | GetStructField (name, field) -> (match envlookup name env with
                                        | StructVal (fields) -> eval field fields
                                        | _ -> raiseErr [name; "is not a struct!"])
    | GetTableVal (name, i) -> (match envlookup name env with
                                 | TableVal (x) -> (match eval i env with 
                                                      | NumberVal (id) -> envgettableval x id
                                                      | _ -> raiseErr ["table index must be a number!"])
                                 | _ -> raiseErr [name; "is not a table!"])
and interp progtree env =
  let (numel, _) = env in
  match progtree with
    | Skip -> env
    | Comb (l, r) -> interp r (interp l env)
    | AssignExpr (varname, vval) -> envadd varname (eval vval env) env
    | SubstExpr (varname, vval) -> envsubst varname (eval vval env) env
    | IfExpr (cond, t, f) -> envcut numel (match eval cond env with
                                | NumberVal v -> (if v <> 0 then
                                                   interp t env 
                                                 else
                                                   interp f env)
                                | BoolVal b -> (if b then
                                                  interp t env
                                                else
                                                  interp f env)
                                | _ -> interp f env)
    | WhileExpr (cond, body) -> envcut numel (match eval cond env with
                                  | NumberVal v -> (if v <> 0 then
                                                      interp (WhileExpr (cond, body)) (interp body env)
                                                    else
                                                      env)
                                  | BoolVal b -> (if b then
                                                    interp (WhileExpr (cond, body)) (interp body env)
                                                  else
                                                    env)
                                  | _ -> env)
    | DeclareProcExpr (name, args, returnexpr, body) -> (let newenv = envadd name (Procedure (args, returnexpr, body, ref env)) env in
                                                           match newenv with
                                                             | (_, (_, Procedure(_, _, _, closure))::_) -> begin
                                                                                                                closure := newenv;
                                                                                                                newenv
                                                             end
                                                             | _ -> raiseErr ["Undefined Behavior!"])              
    | DeclareStructExpr (name, fields) -> envadd name (StructTemplate fields) env
    | SubstStructExpr (s, vval) -> envstructsubst s (eval vval env) env
    | DeclareTableExpr (name, size, default) -> (if size < 1 then raiseErr ["Table size must be a positive number!"]
                                                else envadd name (TableVal((envmaketab 0 size (eval default env)))) env)
    | SubstTableExpr (name, i, vval) -> (match eval i env with
                                          | NumberVal (id) -> envtablesubst name id (eval vval env) env
                                          | _ -> raiseErr ["table index must be a number!"]);;

let interp_file filename = 
  interp (Syntax.Parser.parse_file filename) emptyenv;;