open Ast;;
open Environment;;

exception RunTimeError of string;;

let raiseErr msg = raise (RunTimeError (String.concat " " msg))

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
                  | _ -> raiseErr ["Bad argiment in ! operator!"]);;

let rec interp progtree env =
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
                                                  interp f env))
    | WhileExpr (cond, body) -> envcut numel (match eval cond env with
                                  | NumberVal v -> (if v <> 0 then
                                                      interp (WhileExpr (cond, body)) (interp body env)
                                                    else
                                                      env)
                                  | BoolVal b -> (if b then
                                                    interp (WhileExpr (cond, body)) (interp body env)
                                                  else
                                                    env));;

let interp_file filename = 
  interp (Syntax.Parser.parse_file filename) emptyenv;;

