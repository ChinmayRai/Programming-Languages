open List;;

let d = fun
(Some a) -> a;;

type exp = Var of string | T | F | ITE of exp*exp*exp | Const of int | Lambda of exp*exp | App of exp*exp | Plus of exp*exp | Minus of exp*exp | Multiply of exp*exp | Div of exp*exp | Eq of exp*exp | Ls of exp*exp | Gt of exp*exp |Abs of exp | Neg of exp | And of exp*exp | Or of exp*exp | Not of exp | Proj of int*exp  | Tuple of exp list | Def of exp*exp | Let_in_nd exp*exp;;

type opcode = VAR of string | BIND of string | UNBIND of string | CONST of int | BOOL of bool | DEF | PLUS | MINUS | DIV | EQ | LS | GT |MULTIPLY | POW | MOD | ABS | NEG | AND | OR | NOT | CLOS of string*opcode list | RET | APP | COND of opcode list*opcode list | PROJ | TUPLE of opcode list list ;;

type answer = Num of int | Bool of bool | Tuple_answer of answer list | Vclosure of (string -> answer) list * string * opcode list ;;

let deVar a = match a with
Var s -> s;;

let detuple a = match a with
Tuple x -> x;;
let proj (i,e) = nth (detuple e) (i-1);;

exception TypeException;;

let rec compile e = match e with
  Var s -> [VAR s]
| Const n -> [CONST n]
| T -> [BOOL true]
| F -> [BOOL false]
| Abs e -> (compile e) @ [ABS]
| Neg e -> (compile e) @ [NEG]
| Plus (e1,e2) -> (compile e2) @ (compile e1) @ [PLUS]
| Minus (e1,e2) -> (compile e2) @ (compile e1) @ [MINUS]
| Multiply (e1,e2) -> (compile e2) @ (compile e1) @ [MULTIPLY]
| Def (e1,e2) -> (compile e2) @ [DEF] @ (compile e1) 
| Let_in_nd(e1,e2) -> (compile e1) @ [LIE] @ (compile e2)
| Div (e1,e2) -> (compile e2) @ (compile e1) @ [DIV]
| Eq (e1,e2) -> (compile e2) @ (compile e1) @ [EQ]
| Ls (e1,e2) -> (compile e2) @ (compile e1) @ [LS]
| Gt (e1,e2) -> (compile e2) @ (compile e1) @ [GT]
| Not e -> (compile e) @ [NOT]
| And (e1,e2) -> (compile e2) @ (compile e1) @ [AND]
| Or (e1,e2) -> (compile e2) @ (compile e1) @ [OR]
| Lambda (a,b) -> [CLOS(deVar a,(compile b)@[RET])]
| App (a,b) -> (compile a)@(compile b)@[APP]
| ITE (a,b,c) -> (compile a)@[COND(compile b,compile c)]
| Proj (i,e) -> (compile e) @ [CONST i] @ [PROJ]
| Tuple l -> [ TUPLE (map compile l) ] ;;

let rec valu a (b::c) = 
try Some (b a)
with Match_failure(_,_,_) -> (valu a c);;

let value a (b::c) =d (valu a (b::c));;

let abs e = match e with
Num n -> if n>=0 then Num n
            else Num (-n)
| _ -> raise TypeException;;

let neg e = match e with
Num n -> Num (-n)
| _ -> raise TypeException;;

let not e = match e with 
Bool a -> if a=true then Bool false
		  else Bool true
| _ -> raise TypeException;;

let plus (e1,e2) = match (e1,e2) with
(Num m,Num n) -> Num (m+n)
| _ -> raise TypeException;;

let minus (e1,e2) = match (e1,e2) with
(Num a, Num b) -> Num (a-b)
| _ -> raise TypeException;;

let multiply (e1,e2) = match (e1,e2) with
(Num a,Num b) -> Num(a*b)
| _ -> raise TypeException;;

let div (e1,e2) = match (e1,e2) with
(Num a,Num b) -> Num (a/b)
| _ -> raise TypeException;;


let and1 (e1,e2) = match (e1,e2) with
(Bool a, Bool b) -> Bool (a && b)
| _ -> raise TypeException;;

let or1 (e1,e2) = match (e1,e2) with
(Bool a, Bool b) -> Bool (a || b)
| _ -> raise TypeException;;

let lt (e1,e2) = match (e1,e2) with
(Num a,Num b) -> Bool(a<b)
| _ -> raise TypeException;;

let gt (e1,e2) = match (e1,e2) with
(Num a,Num b) -> Bool(a>b)
| _ -> raise TypeException;;

let eq (e1,e2) = match (e1,e2) with
(Num a,Num b) -> Bool(a=b)
| _ -> raise TypeException;;

let rec execute stk tbl opcd dmp = match (stk,opcd,dmp) with
(stk,[],dmp) -> hd stk
| (stk,CONST n::k,dmp) -> execute (Num n::stk) tbl k dmp
| (stk,BOOL p::k,dmp) -> execute (Bool p::stk) tbl k dmp
| (stk,VAR x::k,dmp) -> execute ((value x tbl)::stk) tbl k dmp
| (a::stk,VAR x::DEF::k,dmp) -> execute stk ((fun x-> a)::tbl) k dmp
| (n::s,ABS::k,dmp) -> execute (abs(n)::s) tbl k dmp
| (n::s,NEG::k,dmp) -> execute (neg(n)::s) tbl k dmp
| (n1::n2::stk,PLUS::k,dmp) -> execute (plus(n1,n2)::stk) tbl k dmp
| (n1::n2::s,MINUS::k,dmp) -> execute (minus(n1,n2)::s) tbl k dmp 
| (n1::n2::s,MULTIPLY::k,dmp) -> execute (multiply(n1,n2)::s) tbl k dmp
| (n1::n2::s,DIV::k,dmp) -> execute (div(n1,n2)::s) tbl k dmp
| (n1::n2::s,EQ::k,dmp) -> execute (eq(n1,n2)::s) tbl k dmp
| (n1::n2::s,LS::k,dmp) -> execute (lt(n1,n2)::s) tbl k dmp
| (n1::n2::s,GT::k,dmp) -> execute (gt(n1,n2)::s) tbl k dmp
| (n::s,NOT::k,dmp) -> execute ((not n)::s) tbl k dmp
| (n1::n2::s,AND::k,dmp) -> execute (and1(n1,n2)::s) tbl k dmp
| (n1::n2::s,OR::k,dmp) -> execute (or1(n1,n2)::s) tbl k dmp
| ((Bool true)::stk,COND(a,b)::c,dmp) -> execute stk tbl (a@c) dmp
| ((Bool false)::stk,COND(a,b)::c,dmp) -> execute stk tbl (b@c) dmp
| (a::stk,BIND(x)::c,dmp) -> execute stk ((fun x -> a)::tbl) c dmp
| (stk,CLOS(y,c)::k,dmp) -> execute (Vclosure(tbl,y,c)::stk) tbl k dmp
| (a::Vclosure(t,y,c)::stk,APP::k,dmp) -> execute [] ((fun y -> a)::t) c ((stk,tbl,k)::dmp)
| (a::stk1,RET::k,(stk,t,k1)::dmp) -> execute (a::stk) t k1 dmp
| (s,(TUPLE l)::k,dmp) -> execute (Tuple_answer (map (fun x -> execute [] tbl x []) l )::s) tbl k dmp 
| (Num n::Tuple_answer l::s,PROJ::k,dmp) -> execute ((nth l (n-1) )::s) tbl k dmp;;