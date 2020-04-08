%{
open Ast
%}

%token <int> INT
%token TRUE
%token FALSE
%token IF
%token ELSE
%token WHILE
%token LT
%token LEQ
%token GT
%token GEQ
%token EQ
%token NOT
%token LBRACE
%token RBRACE
%token SEMICOLON
%token <string> ID
%token PLUS
%token MINUS
%token TIMES
%token FRONTSLASH
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token EOF
%token PROCEDURE
%token COMMA
%token COLLON

%right NOT
%left LEQ GEQ LT GT EQ
%left MINUS PLUS
%left FRONTSLASH TIMES

%start <Ast.pexpr> program

%%

aexpr:
  | LPAREN; e = aexpr; RPAREN { e }
  | i = INT { Const (NumberVal i) }
  | TRUE { Const (BoolVal true) }
  | FALSE { Const (BoolVal false) }
  | x = ID { Var x }
  | e1 = aexpr; LT; e2 = aexpr { Lt (e1, e2) }
  | e1 = aexpr; LEQ; e2 = aexpr { Leq (e1, e2) }
  | e1 = aexpr; GT; e2 = aexpr { Gt (e1, e2) }
  | e1 = aexpr; GEQ; e2 = aexpr { Geq (e1, e2) }
  | e1 = aexpr; EQ; e2 = aexpr { Eq (e1, e2) }
  | e1 = aexpr; MINUS; e2 = aexpr { Sub (e1, e2) }
  | e1 = aexpr; PLUS; e2 = aexpr { Add (e1, e2) }
  | e1 = aexpr; FRONTSLASH; e2 = aexpr { Div (e1, e2) }
  | e1 = aexpr; TIMES; e2 = aexpr { Mult (e1, e2) }
  | NOT; e = aexpr { Not e }
  | x = ID; al = argslist { EvalProc (x, al) }

argslist:
  | LPAREN; RPAREN { [] }
  | LPAREN; a = args; RPAREN { a }

args:
  | e = aexpr { [e] }
  | e = aexpr; COMMA; a = args { e::a }

pexpr: 
  | LET; x = ID; EQUALS; e1 = aexpr; SEMICOLON { AssignExpr (x, e1) }
  | x = ID; EQUALS; e = aexpr; SEMICOLON { SubstExpr (x, e) }
  | IF; e = aexpr; e1 = block; ELSE; e2 = block; SEMICOLON { IfExpr (e, e1, e2) }
  | WHILE; e = aexpr; e1 = block; SEMICOLON { WhileExpr (e, e1) }
  | PROCEDURE; x = ID; dal = dargslist; COLLON; e = aexpr; e1 = block; SEMICOLON { DeclareProcExpr (x, dal, e, e1) }
 
dargslist: 
  | LPAREN; RPAREN { [] }
  | LPAREN; da = dargs; RPAREN { da }

dargs:
  | x = ID { [x] }
  | x = ID; COMMA; da = dargs { x::da } 

block:
  | LBRACE; RBRACE { Skip }
  | LBRACE; c = commands; RBRACE { c }

commands:
  | e = pexpr { e }
  | e = pexpr; c = commands { Comb (e, c) }

program:
  | e = pexpr; p = program { Comb (e, p) }
  | e = pexpr; EOF { e }