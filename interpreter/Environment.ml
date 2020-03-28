open Ast;;

exception EnvironmentError of string;;

let raiseErr msg = raise (EnvironmentError (String.concat " " msg))

type env = (string * value) list;;

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

let rec envcut numel (actnumel, env) = if numel = actnumel then 
                                         (numel,  env)
                                       else
                                         match env with
                                           | [] -> raiseErr ["Run Time Error"]
                                           | (_::rest) -> envcut numel (actnumel-1, rest);; 
