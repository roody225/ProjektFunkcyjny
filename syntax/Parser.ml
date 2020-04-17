
let parse lexbuf = 
  let ast = Grammar.program Lexer.read lexbuf in
  ast

let parse_file filename =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  let result = parse lexbuf in
  close_in file;
  result;;

let parse_string source =
  let lexbuf = Lexing.from_string source in
  parse lexbuf;;