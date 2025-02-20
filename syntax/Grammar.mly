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
%token LSQBRACE
%token RSQBRACE
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
%token STRUCT
%token DOT
%token NULL
%token WITH
%token QUOTES
%token TABLE
%token PRINT
%token PUTENDL
%token PUTSPACE
%token READINT
%token READSTRING
%token FREEVAR
%token FREEALL
%token FOR

%right NOT
%left LEQ GEQ LT GT EQ
%left MINUS PLUS
%left FRONTSLASH TIMES

%start <Ast.pexpr> program

%%

aexpr:
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
  | x = ID; WITH; al = argslist { MakeStruct (x, al) }
  | x = getstructf; { x }
  | NULL { Const (Null) }
  | i = INT { Const (NumberVal i) }
  | TRUE { Const (BoolVal true) }
  | FALSE { Const (BoolVal false) }
  | QUOTES; s = ID; QUOTES { Const (StringVal s) }
  | x = ID; LSQBRACE; i = aexpr; RSQBRACE { GetTableVal (x, i) }
  | x = ID { Var x }
  | LPAREN; e = aexpr; RPAREN { e }

getstructf:
  | x = ID; DOT; y = ID {GetStructField (x, Var y)}
  | x = ID; DOT; e = getstructf { GetStructField (x, e) }

argslist:
  | LPAREN; RPAREN { [] }
  | LPAREN; a = args; RPAREN { a }

args:
  | e = aexpr { [e] }
  | e = aexpr; COMMA; a = args { e::a }

pexpr: 
  | LET; x = ID; EQUALS; e1 = aexpr; SEMICOLON { AssignExpr (x, e1) }
  | x = ID; EQUALS; e = aexpr; SEMICOLON { SubstExpr (x, e) }
  | x = getstructf; EQUALS; e = aexpr; SEMICOLON { SubstStructExpr (x, e) }
  | x = ID; LSQBRACE; i = aexpr; RSQBRACE; EQUALS; e = aexpr; SEMICOLON { SubstTableExpr (x, i, e) } 
  | IF; e = aexpr; e1 = block; ELSE; e2 = block { IfExpr (e, e1, e2) }
  | WHILE; e = aexpr; e1 = block { WhileExpr (e, e1) }
  | FOR; x = ID; EQUALS; e = aexpr; SEMICOLON; e1 = aexpr; SEMICOLON; e2 = pexpr; e3 = block { ForExpr (x, e, e1, e2, e3) }
  | PROCEDURE; x = ID; dal = dargslist; COLLON; e = aexpr; e1 = block { DeclareProcExpr (x, dal, e, e1) }
  | STRUCT; x = ID; sb = structbody { DeclareStructExpr (x, sb) }
  | TABLE; LPAREN; x = ID; COMMA; i = aexpr; COMMA; e = aexpr; RPAREN; SEMICOLON { DeclareTableExpr (x, i, e) }
  | PRINT; al = argslist; SEMICOLON { PrintExpr (al) }
  | PUTENDL; LPAREN; RPAREN; SEMICOLON { PutEndlExpr }
  | PUTSPACE; LPAREN; RPAREN; SEMICOLON { PutSpaceExpr }
  | READINT; LPAREN; x = ID; RPAREN; SEMICOLON { ReadIntExpr (x) }
  | READSTRING; LPAREN; x = ID; RPAREN; SEMICOLON { ReadStringExpr (x) }
  | FREEVAR; LPAREN; x = ID; RPAREN; SEMICOLON { FreeVarExpr (x) }
  | FREEALL; LPAREN; RPAREN; SEMICOLON { FreeAllExpr }

structbody:
  | LBRACE; RBRACE { [] }
  | LBRACE; m = structmembers; RBRACE { m }

structmembers:
  | x = ID { [x] }
  | x = ID; COMMA; rest = structmembers { x::rest }
 
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