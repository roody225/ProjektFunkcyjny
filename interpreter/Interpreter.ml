open Ast;;
open Environment;;

exception RunTimeError of string;;

let raiseErr msg = raise (RunTimeError (String.concat " " msg))

let rec mapargs argslist argsvals env =
  let (numel, vars) = env in
  match (argslist, argsvals) with
    | ([], []) -> env
    | (name::alist, vval::aval) -> mapargs alist aval (numel+1, (name, vval)::vars)
    | (_, _) -> raiseErr ["too few/many arguments in a function!"];;

let rec eval expr env = 
  match expr with
    | Const c -> c
    | Var v -> envlookup v env
    | Add (l, r) -> (match (eval l env, eval r env) with
                      | (NumberVal left, NumberVal right) -> NumberVal (left + right)
                      | _ -> raiseErr ["Bad argument in + operator!"])
    | Sub (l, r) -> (match (eval l env, eval r env) with
                      | (NumberVal left, NumberVal right) -> NumberVal (left - right)
                      | _ -> raiseErr ["Bad argument in - operator!"])
    | Div (l, r) -> (match (eval l env, eval r env) with
                      | (NumberVal left, NumberVal right) -> (if right=0 then 
                                                               raiseErr ["division by 0!"]
                                                             else 
                                                               NumberVal (left + right))
                      | _ -> raiseErr ["Bad argument in / operator!"])
    | Mult (l, r) -> (match (eval l env, eval r env) with
                      | (NumberVal left, NumberVal right) -> NumberVal (left * right)
                      | _ -> raiseErr ["Bad argument in * operator!"])
    | Lt (l, r) -> (match (eval l env, eval r env) with
                      | (NumberVal left, NumberVal right) -> BoolVal (left < right)
                      | (BoolVal left, BoolVal right) -> BoolVal (left < right)
                      | _ -> raiseErr ["Bad argument in < operator!"])     
    | Leq (l, r) -> (match (eval l env, eval r env) with
                      | (NumberVal left, NumberVal right) -> BoolVal (left <= right)
                      | (BoolVal left, BoolVal right) -> BoolVal (left <= right)
                      | _ -> raiseErr ["Bad argument in <= operator!"])    
    | Gt (l, r) -> (match (eval l env, eval r env) with
                      | (NumberVal left, NumberVal right) -> BoolVal (left > right)
                      | (BoolVal left, BoolVal right) -> BoolVal (left > right)
                      | _ -> raiseErr ["Bad argument in > operator!"])   
    | Geq (l, r) -> (match (eval l env, eval r env) with
                      | (NumberVal left, NumberVal right) -> BoolVal (left >= right)
                      | (BoolVal left, BoolVal right) -> BoolVal (left >= right)
                      | _ -> raiseErr ["Bad argument in >= operator!"])
    | Eq (l, r) -> (match (eval l env, eval r env) with
                      | (NumberVal left, NumberVal right) -> BoolVal (left = right)
                      | (BoolVal left, BoolVal right) -> BoolVal (left = right)
                      | _ -> raiseErr ["Bad argument in == operator!"])
    | Not e -> (match eval e env with 
                  | BoolVal b -> BoolVal (not b)
                  | _ -> raiseErr ["Bad argiment in ! operator!"])
    | EvalProc (name, argsvals) -> (match envlookup name env with 
                                     | Procedure (argslist, returnval, body, closure) ->
                                       eval returnval (interp body (mapargs argslist (List.map (fun a -> eval a env) argsvals) closure))
                                     | _ -> raiseErr [name; "is not a procedure!"])
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
                                | Procedure _ -> raiseErr ["Run Time Error"])
    | WhileExpr (cond, body) -> envcut numel (match eval cond env with
                                  | NumberVal v -> (if v <> 0 then
                                                      interp (WhileExpr (cond, body)) (interp body env)
                                                    else
                                                      env)
                                  | BoolVal b -> (if b then
                                                    interp (WhileExpr (cond, body)) (interp body env)
                                                  else
                                                    env)
                                  | Procedure _ -> raiseErr ["Run Time Error"])
    | DeclareProcExpr (name, args, returnexpr, body) -> envadd name (Procedure (args, returnexpr, body, env)) env;;

let interp_file filename = 
  interp (Syntax.Parser.parse_file filename) emptyenv;;