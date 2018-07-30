type exp = Var of string | Const of int | Abs of exp | Neg of exp | Plus of exp*exp | Minus of exp*exp | Multiply of exp*exp | Div of exp*exp | Mod of exp*exp | Pow of exp*exp | T | F | Not of exp | And of exp*exp | Or of exp*exp | Imply of exp*exp | Lt of exp*exp | Gt of exp*exp | Eq of exp*exp | Le of exp*exp | Ge of exp*exp | Proj of int*exp  | Tuple of exp list;;

type answer = Number of int | Boolean of bool | Tuple_answer of answer list;;

let detuple a = match a with
Tuple x -> x;;

let rec power = function
(a,b) -> if b=0 then 1
         else if b=1 then a
         else  a * power(a,b-1);;

let negate x = match x with
| true -> false
| false -> true;;

exception TypeException;;

open List;;

let proj (i,e) = nth (detuple e) (i-1);;

let abs e = match e with
Number n -> if n>=0 then Number n
            else Number (-n)
| _ -> raise TypeException;;

let neg e = match e with
Number n -> Number (-n)
| _ -> raise TypeException;;

let plus (e1,e2) = match (e1,e2) with
(Number m,Number n) -> Number (m+n)
| _ -> raise TypeException;;

let minus (e1,e2) = match (e1,e2) with
(Number a, Number b) -> Number (a-b)
| _ -> raise TypeException;;

let multiply (e1,e2) = match (e1,e2) with
(Number a,Number b) -> Number(a*b)
| _ -> raise TypeException;;

let div (e1,e2) = match (e1,e2) with
(Number a,Number b) -> Number (a/b)
| _ -> raise TypeException;;

let modulus (e1,e2) = match (e1,e2) with
(Number a,Number b) -> Number (a mod b)
| _ -> raise TypeException;;

let pow (e1,e2) = match (e1,e2) with
(Number a,Number b) ->  Number (power (a,b))
| _ -> raise TypeException;;

let not e = match e with 
Boolean a -> Boolean (negate(a))
| _ -> raise TypeException;;

let and1 (e1,e2) = match (e1,e2) with
(Boolean a, Boolean b) -> Boolean (a && b)
| _ -> raise TypeException;;

let or1 (e1,e2) = match (e1,e2) with
(Boolean a, Boolean b) -> Boolean (a || b)
| _ -> raise TypeException;;

let imlp (a,b) = match (a,b) with
(true,false) -> false
| _ -> true;;

let imply (e1,e2) = match (e1,e2) with
(Boolean a,Boolean b) -> Boolean (imlp (a,b))
| _ -> raise TypeException;;

let lt (e1,e2) = match (e1,e2) with
(Number a,Number b) -> Boolean(a<b)
| _ -> raise TypeException;;


let gt (e1,e2) = match (e1,e2) with
(Number a,Number b) -> Boolean(a>b)
| _ -> raise TypeException;;


let ge (e1,e2) = match (e1,e2) with
(Number a,Number b) -> Boolean(a>=b)
| _ -> raise TypeException;;

let le (e1,e2) = match (e1,e2) with
(Number a,Number b) -> Boolean(a<=b)
| _ -> raise TypeException;;

let eq (e1,e2) = match (e1,e2) with
(Number a,Number b) -> Boolean(a=b)
| _ -> raise TypeException;;

open String;;

let table s =  Number (length s);;   (*This is an example of kind of table function that we can use.*)

let rec eval e = match e with 
 Const n -> Number n
| Var x -> table x 
| Abs n -> abs (eval n)
| Neg n -> neg (eval n)
| Plus (e1,e2) -> plus(eval e1,eval e2)
| Minus (e1,e2) -> minus(eval e1,eval e2)
| Multiply (e1,e2) -> multiply((eval e1),(eval e2))
| Div (e1,e2) -> div ((eval e1),(eval e2))
| Mod (e1,e2) -> modulus((eval e1),(eval e2))
| Pow (e1,e2) -> pow((eval e1),(eval e2))
| T -> Boolean true
| F -> Boolean false 
| Not e -> not (eval e)
| And (e1,e2) -> and1((eval e1),(eval e2))
| Or (e1,e2) -> or1((eval e1),(eval e2))
| Imply (e1,e2) -> imply((eval e1),(eval e2))
| Lt (e1,e2) -> lt((eval e1),(eval e2))
| Gt (e1,e2) -> gt((eval e1),(eval e2))
| Le (e1,e2) -> le((eval e1),(eval e2))
| Ge (e1,e2) -> ge((eval e1),(eval e2))
| Eq (e1,e2) -> eq((eval e1),(eval e2))
| Proj (i,e)-> eval(proj(i,e))
| Var (x) -> table x;;


type opcode = VAR of string | CONST of int | BOOLEAN of bool | ABS | NEG | PLUS | MINUS | MULTIPLY | DIV | MOD | POW | NOT | AND | OR | IMPLY | LT | GT | LE | GE | EQ | PROJ | TUPLE of opcode list list ;;

open List;;

let rec compilelist f l = match l with
[] -> []
| a::b -> (f a) @ (compilelist f b) ;;

let rec compile e = match e with
  Var s -> [VAR s]
| Const n -> [CONST n]
| T -> [BOOLEAN true]
| F -> [BOOLEAN false]
| Abs e -> (compile e) @ [ABS]
| Neg e -> (compile e) @ [NEG]
| Plus (e1,e2) -> (compile e2) @ (compile e1) @ [PLUS]
| Minus (e1,e2) -> (compile e2) @ (compile e1) @ [MINUS]
| Multiply (e1,e2) -> (compile e2) @ (compile e1) @ [MULTIPLY]
| Div (e1,e2) -> (compile e2) @ (compile e1) @ [DIV]
| Mod (e1,e2) -> (compile e2) @ (compile e1) @ [MOD]
| Pow (e1,e2) -> (compile e2) @ (compile e1) @ [POW]
| Not e -> (compile e) @ [NOT]
| And (e1,e2) -> (compile e2) @ (compile e1) @ [AND]
| Or (e1,e2) -> (compile e2) @ (compile e1) @ [OR]
| Imply (e1,e2) -> (compile e2) @ (compile e1) @ [IMPLY]
| Lt (e1,e2) -> (compile e2) @ (compile e1) @ [LT]
| Gt (e1,e2) -> (compile e2) @ (compile e1) @ [GT]
| Le (e1,e2) -> (compile e2) @ (compile e1) @ [LE]
| Ge (e1,e2) -> (compile e2) @ (compile e1) @ [GE]
| Eq (e1,e2) -> (compile e2) @ (compile e1) @ [EQ]
| Proj (i,e) -> (compile e) @ [CONST i] @ [PROJ]
| Tuple l -> [ TUPLE (map compile l) ] ;;
  
  
  let rec execute s t l = match (s,l) with
    (s,[]) -> hd s
  | (s,CONST n::k) -> execute (Number n::s) t k
  | (s,BOOLEAN p::k) -> execute (Boolean p::s) t k
  | (s,VAR x::k) -> execute ((t x)::s) t k
  | (n::s,ABS::k) -> execute (abs(n)::s) t k
  | (n::s,NEG::k) -> execute (neg(n)::s) t k
  | (n1::n2::s,PLUS::k) -> execute (plus(n1,n2)::s) t k
  | (n1::n2::s,MINUS::k) -> execute (minus(n1,n2)::s) t k
  | (n1::n2::s,MULTIPLY::k) -> execute (multiply(n1,n2)::s) t k
  | (n1::n2::s,DIV::k) -> execute (div(n1,n2)::s) t k
  | (n1::n2::s,MOD::k) -> execute (modulus(n1,n2)::s) t k
  | (n1::n2::s,POW::k) -> execute (pow(n1,n2)::s) t k
  | (n::s,NOT::k) -> execute ((not n)::s) t k
  | (n1::n2::s,AND::k) -> execute (and1(n1,n2)::s) t k
  | (n1::n2::s,OR::k) -> execute (or1(n1,n2)::s) t k
  | (n1::n2::s,IMPLY::k) -> execute (imply(n1,n2)::s) t k
  | (n1::n2::s,LT::k) -> execute (lt(n1,n2)::s) t k
  | (n1::n2::s,GT::k) -> execute (gt(n1,n2)::s) t k
  | (n1::n2::s,LE::k) -> execute (le(n1,n2)::s) t k
  | (n1::n2::s,GE::k) -> execute (ge(n1,n2)::s) t k
  | (n1::n2::s,EQ::k) -> execute (eq(n1,n2)::s) t k
  | (s,(TUPLE l)::k) -> execute (Tuple_answer (map (execute [] t) l )::s) t k
  | (Number n::Tuple_answer l::s,PROJ::k) -> execute ((nth l (n-1) )::s) t k;;



let a = Plus(Multiply(Abs (Const (-3)),Neg (Const (-5))),Minus(Pow(Const 3,Const 2),Div(Const 16,Mod(Const 9,Const 5))));;

eval a = execute [] table (compile a);; 

let b = Imply(And(Lt(Const 3,Const 4),Gt(Const 2,Const (-2))),Not(Or(Ge(Neg(Const 4),Const 2),Le(Abs(Const(-2)),Const (-4)))));;

eval b = execute [] table (compile b);;

let c = Proj(2,Tuple[Plus(Const 4,Minus(Const 5,Const 2));Multiply(Pow (Const 3,Const 2),Mod(Const 5,Const 3));And(Gt (Const 4,Const 5),Lt(Const 4,Const 5))]);;

eval c = execute [] table (compile c);;

let d = Plus(Plus(Pow(Var "abcd",Const 2),Pow(Var "efg",Const 2)),Multiply(Multiply(Var "abcd",Var "efg"),Const 2));;

eval d = execute [] table (compile d);;

let e = Proj(2,Tuple[And(Eq(Var "abcd",Var "efgh"),Ge(Var "abcde",Const 5));Proj(2,Tuple[Plus(Const 5,Const 6);Or(And(T,F),Or(T,F));Minus(Const 4,Const 1)]);Pow(Const 3,Const 2)]);;

eval e = execute [] table (compile e);;

let f = Imply( Eq(Pow(Plus(Const 3,Const 2),Div(Const 4,Const 2)),Plus(Pow(Const 4,Const 2),Mod(Const 20,Const 11))),Or(Not(Eq(Abs(Const (-5)),Const 5)),Ge(Neg(Const 3),Const 4)));;

eval f = execute [] table (compile f);;
