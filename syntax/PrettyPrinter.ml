open Ast;;

let unparsev value =
  match value with
    | Null -> "null"
    | NumberVal (c) -> string_of_int c
    | BoolVal (b) -> string_of_bool b
    | StringVal (s) -> String.concat "" ["\""; s; "\""]
    | _ -> "something went wrong";;

let rec unparsea expr = 
  match expr with
    | Var (name) -> name
    | Const (value) -> unparsev value
    | Add (e1, e2) -> String.concat "" ["("; unparsea e1; " + "; unparsea e2; ")"]
    | Sub (e1, e2) -> String.concat "" ["("; unparsea e1; " - "; unparsea e2; ")"]
    | Mult (e1, e2) -> String.concat "" ["("; unparsea e1; " * "; unparsea e2; ")"]
    | Div (e1, e2) -> String.concat "" ["("; unparsea e1; " / "; unparsea e2; ")"]
    | Lt (e1, e2) -> String.concat "" ["("; unparsea e1; " < "; unparsea e2; ")"]
    | Leq (e1, e2) -> String.concat "" ["("; unparsea e1; " <= "; unparsea e2; ")"]
    | Gt (e1, e2) -> String.concat "" ["("; unparsea e1; " > "; unparsea e2; ")"]
    | Geq (e1, e2) -> String.concat "" ["("; unparsea e1; " >= "; unparsea e2; ")"]
    | Eq (e1, e2) -> String.concat "" ["("; unparsea e1; " == "; unparsea e2; ")"]
    | Not (e) -> String.concat "" ["!("; unparsea e; ")"]
    | EvalProc (name, args) -> String.concat "" [name; "("; String.concat ", " (List.map unparsea args); ")"]
    | MakeStruct (name, vals) -> String.concat "" [name; " with ("; String.concat ", " (List.map unparsea vals); ")"]
    | GetStructField (name, field) -> String.concat "" [name; "."; unparsea field]
    | GetTableVal (name, id) -> String.concat "" [name; "["; unparsea id; "]"];;

let newp p = String.concat "" [p; "  "];;

let rec unparsep program prefix = 
  match program with
    | Skip -> " "
    | Comb (e1, e2) -> String.concat "" [unparsep e1 prefix; unparsep e2 prefix;]
    | DeclareStructExpr (name, fields) -> String.concat "" [prefix; "struct "; name; " {"; String.concat ", " fields; "}\n"]
    | DeclareProcExpr (name, args, return, block) -> String.concat "" [prefix; "procedure "; name; "("; String.concat ", " args; ") : "; unparsea return; " {\n"; unparsep block (newp prefix); prefix; "}\n"]
    | WhileExpr (cond, block) -> String.concat "" [prefix; "while "; unparsea cond; " {\n"; unparsep block (newp prefix); prefix; "}\n"]
    | ForExpr (iter, vval, cond, modf, block) -> String.concat "" [prefix; "for "; iter; " = "; unparsea vval; "; "; unparsea cond; "; "; unparsep modf ""; " {\n"; unparsep block (newp prefix); prefix; "}\n"]
    | IfExpr (cond, t, f) -> String.concat "" [prefix; "if "; unparsea cond; " {\n"; unparsep t (newp prefix); prefix; "}\n"; prefix; "else {\n"; unparsep f (newp prefix); prefix; "}\n"]
    | SubstExpr (name, value) -> String.concat "" [prefix; name; " = "; unparsea value; ";\n"]
    | AssignExpr (name, value) -> String.concat "" [prefix; "let "; name; " = "; unparsea value; ";\n"]
    | SubstStructExpr (s, value) -> String.concat "" [prefix; unparsea s; " = "; unparsea value; ";\n"]
    | DeclareTableExpr (name, size, default) -> String.concat "" [prefix; "table("; name; ", "; unparsea size; ", "; unparsea default; ");\n"]
    | SubstTableExpr (name, i, value) -> String.concat "" [prefix; name; "["; unparsea i; "] = "; unparsea value; ";\n"]
    | PrintExpr (argsvals) -> String.concat "" [prefix; "print("; String.concat ", " (List.map unparsea argsvals); ");\n"]
    | PutEndlExpr -> String.concat "" [prefix; "putendl();\n"]
    | PutSpaceExpr -> String.concat "" [prefix; "putspace();\n"]
    | ReadIntExpr (name) -> String.concat "" [prefix; "readint("; name; ");\n"]
    | ReadStringExpr (name) -> String.concat "" [prefix; "readstr("; name; ");\n"]
    | FreeVarExpr (name) -> String.concat "" [prefix; "freevar("; name; ");\n"]
    | FreeAllExpr -> String.concat "" [prefix; "freeall();\n"]
    | _ -> "something went wrong";;

let prettyprint p = Printf.printf "%s" (unparsep p "");;