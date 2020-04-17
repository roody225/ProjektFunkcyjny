# ProjektFunkcyjny

install ocaml

install opam

opam install dune menhir utop

----------------------------------------------------

make debug - in main directory to run utop

Syntax.Parser.parse_file "path_to_file";; - to parse file
Syntax.PP.prettyprint parsed_program;; - to pretty print parsed program

Eval.Interpreter.interp_file "path_to_file";; - to interpret file

----------------------------------------------------

integer =/= 0 and bool value true == True
any other value == False