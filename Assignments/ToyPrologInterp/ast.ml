type symbol = string*int;;
type term = Var of string | Const of string | N of symbol*term list | FAIL | CUT;;
type clause = Fact of term | Rule of term*term list;;    (*The Fact contains one atom for head.The Rule contains an atom for head and an atom list for body*)
type program = clause list;;