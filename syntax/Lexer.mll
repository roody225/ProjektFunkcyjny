{
open Grammar
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read = 
  parse
  | white { read lexbuf }
  | "+"   { PLUS }
  | "-"   { MINUS }
  | "/"   { FRONTSLASH }
  | "*"   { TIMES }
  | "<"   { LT }
  | "<="  { LEQ }
  | ">"   { GT }
  | ">="  { GEQ }
  | "=="  { EQ }
  | "("   { LPAREN }
  | ")"   { RPAREN }
  | "let" { LET }
  | "="   { EQUALS }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if"  { IF }
  | "else" { ELSE }
  | "while" { WHILE }
  | ";"   { SEMICOLON }
  | "!"   { NOT }
  | "{"   { LBRACE }
  | "}"   { RBRACE }
  | "procedure" { PROCEDURE }
  | ","   { COMMA }
  | ":"   { COLLON }
  | "struct" { STRUCT }
  | "."   { DOT }
  | "null" { NULL }
  | "with" { WITH }
  | "\""  { QUOTES }
  | id    { ID (Lexing.lexeme lexbuf) }
  | int   { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof   { EOF }
   