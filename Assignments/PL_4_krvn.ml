open List;;

exception TypeException;;

type exp = Var of string | T | F | Hole | ITE of exp*exp*exp | Const of int | Plus of exp*exp | Minus of exp*exp | Multiply of exp*exp | Div of exp*exp | Eq of exp*exp | Ls of exp*exp | Gt of exp*exp | Abs of exp | Neg of exp | Not of exp | And of exp*exp | Or of exp*exp | Lambda of exp*exp | App of exp*exp | Proj of int*exp | Tuple of exp list | Tuple_compute of exp list * exp list | Closure of (string -> exp) list * exp ;;

type answer = Num of int | Bool of bool | Tuple_answer of answer list;;

let d = fun
(Some a) -> a;;

let rec valu a (b::c) = 
try Some (b a)
with Match_failure(_,_,_) -> (valu a c);;

let value a b = d(valu a b);;

let rec executeK a b = match (a,b) with
(Closure(t,T),[]) -> Closure(t,T)
| (Closure(t,F),[]) -> Closure(t,F)
| (Closure(t,Const n),[]) -> Closure(t,Const n)
| (Closure(t,Tuple l),[]) -> Closure(t,Tuple l)

| (Closure(t,Abs(e1)),stk) -> executeK (Closure(t,e1)) (Closure(t,Abs(Hole))::stk)
| (Closure(t1,Const n1),Closure(t,Abs(Hole))::stk) -> if n1>=0 then executeK (Closure(t,Const n1)) stk else executeK (Closure(t,Const (-n1))) stk

| (Closure(t,Neg(e1)),stk) -> executeK (Closure(t,e1)) (Closure(t,Neg(Hole))::stk)
| (Closure(t1,Const n1),Closure(t,Neg(Hole))::stk) -> executeK (Closure(t,Const (-n1))) stk

| (Closure(t,Not(e1)),stk) -> executeK (Closure(t,e1)) (Closure(t,Not(Hole))::stk)
| (Closure(t1,T),Closure(t,Not(Hole))::stk) -> executeK (Closure(t,F)) stk
| (Closure(t1,F),Closure(t,Not(Hole))::stk) -> executeK (Closure(t,T)) stk

| (Closure(t,Plus(e1,e2)),stk) -> executeK (Closure(t,e1)) (Closure(t,Plus(Hole,e2))::stk)
| (Closure(t1,Const n1),(Closure(t,Plus(Hole,e2))::stk)) -> executeK (Closure(t,e2)) (Closure(t,Plus(Const n1,Hole))::stk)
| (Closure(t1,Const n2),Closure(t,Plus(Const n1,Hole))::stk) -> executeK (Closure(t,Const (n1+n2))) stk

| (Closure(t,Minus(e1,e2)),stk) -> executeK (Closure(t,e1)) (Closure(t,Minus(Hole,e2))::stk)
| (Closure(t1,Const n1),(Closure(t,Minus(Hole,e2))::stk)) -> executeK (Closure(t,e2)) (Closure(t,Minus(Const n1,Hole))::stk)
| (Closure(t1,Const n2),Closure(t,Minus(Const n1,Hole))::stk) -> executeK (Closure(t,Const (n1-n2))) stk

| (Closure(t,Multiply(e1,e2)),stk) -> executeK (Closure(t,e1)) (Closure(t,Multiply(Hole,e2))::stk)
| (Closure(t1,Const n1),(Closure(t,Multiply(Hole,e2))::stk)) -> executeK (Closure(t,e2)) (Closure(t,Multiply(Const n1,Hole))::stk)
| (Closure(t1,Const n2),Closure(t,Multiply(Const n1,Hole))::stk) -> executeK (Closure(t,Const (n1*n2))) stk

| (Closure(t,Div(e1,e2)),stk) -> executeK (Closure(t,e1)) (Closure(t,Div(Hole,e2))::stk)
| (Closure(t1,Const n1),(Closure(t,Div(Hole,e2))::stk)) -> executeK (Closure(t,e2)) (Closure(t,Div(Const n1,Hole))::stk)
| (Closure(t1,Const n2),Closure(t,Div(Const n1,Hole))::stk) -> executeK (Closure(t,Const (n1/n2))) stk

| (Closure(t,Eq(e1,e2)),stk) -> executeK (Closure(t,e1)) (Closure(t,Eq(Hole,e2))::stk)
| (Closure(t1,Const n1),(Closure(t,Eq(Hole,e2))::stk)) -> executeK (Closure(t,e2)) (Closure(t,Eq(Const n1,Hole))::stk)
| (Closure(t1,Const n2),Closure(t,Eq(Const n1,Hole))::stk) -> if n1=n2 then executeK (Closure(t,T)) stk else executeK (Closure(t,F)) stk

| (Closure(t,Ls(e1,e2)),stk) -> executeK (Closure(t,e1)) (Closure(t,Ls(Hole,e2))::stk)
| (Closure(t1,Const n1),(Closure(t,Ls(Hole,e2))::stk)) -> executeK (Closure(t,e2)) (Closure(t,Ls(Const n1,Hole))::stk)
| (Closure(t1,Const n2),Closure(t,Ls(Const n1,Hole))::stk) -> if n1<n2 then executeK (Closure(t,T)) stk else executeK (Closure(t,F)) stk

| (Closure(t,Gt(e1,e2)),stk) -> executeK (Closure(t,e1)) (Closure(t,Gt(Hole,e2))::stk)
| (Closure(t1,Const n1),(Closure(t,Gt(Hole,e2))::stk)) -> executeK (Closure(t,e2)) (Closure(t,Gt(Const n1,Hole))::stk)
| (Closure(t1,Const n2),Closure(t,Gt(Const n1,Hole))::stk) -> if n1>n2 then executeK (Closure(t,T)) stk else executeK (Closure(t,F)) stk

| (Closure(t,And(e1,e2)),stk) -> executeK (Closure(t,e1)) (Closure(t,And(Hole,e2))::stk)
| (Closure(t1,T),(Closure(t,And(Hole,e2))::stk)) -> executeK (Closure(t,e2)) (Closure(t,And(T,Hole))::stk)
| (Closure(t1,F),(Closure(t,And(Hole,e2))::stk)) -> executeK (Closure(t,e2)) (Closure(t,And(F,Hole))::stk)
| (Closure(t1,T),Closure(t,And(T,Hole))::stk) -> executeK (Closure(t,T)) stk
| (Closure(t1,F),Closure(t,And(T,Hole))::stk) -> executeK (Closure(t,F)) stk
| (Closure(t1,T),Closure(t,And(F,Hole))::stk) -> executeK (Closure(t,F)) stk
| (Closure(t1,F),Closure(t,And(F,Hole))::stk) -> executeK (Closure(t,F)) stk

| (Closure(t,Or(e1,e2)),stk) -> executeK (Closure(t,e1)) (Closure(t,Or(Hole,e2))::stk)
| (Closure(t1,T),(Closure(t,Or(Hole,e2))::stk)) -> executeK (Closure(t,e2)) (Closure(t,Or(T,Hole))::stk)
| (Closure(t1,F),(Closure(t,Or(Hole,e2))::stk)) -> executeK (Closure(t,e2)) (Closure(t,Or(F,Hole))::stk)
| (Closure(t1,T),Closure(t,Or(T,Hole))::stk) -> executeK (Closure(t,T)) stk
| (Closure(t1,F),Closure(t,Or(T,Hole))::stk) -> executeK (Closure(t,T)) stk
| (Closure(t1,T),Closure(t,Or(F,Hole))::stk) -> executeK (Closure(t,T)) stk
| (Closure(t1,F),Closure(t,Or(F,Hole))::stk) -> executeK (Closure(t,F)) stk

| (Closure(t,ITE(e1,e2,e3)),stk) -> executeK (Closure(t,e1)) (Closure(t,ITE(Hole,e2,e3))::stk)
| (Closure(t1,T),(Closure(t,ITE(Hole,e2,e3))::stk)) -> executeK (Closure(t,e2)) stk
| (Closure(t1,F),(Closure(t,ITE(Hole,e2,e3))::stk)) -> executeK (Closure(t,e3)) stk

| (Closure(t,Proj(n,e)),stk) -> executeK (Closure(t,e)) (Closure(t,Proj(n,Hole))::stk)
| (Closure(t1,Tuple l),(Closure(t,Proj(n,Hole))::stk)) -> executeK (Closure(t,nth l (n-1))) stk

| (Closure(t,Tuple (a::b)),stk) -> executeK (Closure(t,a)) (Closure(t,Tuple_compute([Hole],b))::stk)
| (Closure(t1,Const n),Closure(t,Tuple_compute([Hole],b::c))::stk) -> executeK (Closure(t,b)) (Closure(t,Tuple_compute([Const n],c))::stk)
| (Closure(t1,T),Closure(t,Tuple_compute([Hole],b::c))::stk) -> executeK (Closure(t,b)) (Closure(t,Tuple_compute([T],c))::stk)
| (Closure(t1,F),Closure(t,Tuple_compute([Hole],b::c))::stk) -> executeK (Closure(t,b)) (Closure(t,Tuple_compute([F],c))::stk)

| (Closure(t1,Const n),Closure(t,Tuple_compute(a,b::c))::stk) -> executeK (Closure(t,b)) (Closure(t,Tuple_compute(a@[Const n],c))::stk)
| (Closure(t1,T),Closure(t,Tuple_compute(a,b::c))::stk) -> executeK (Closure(t,b)) (Closure(t,Tuple_compute(a@[T],c))::stk)
| (Closure(t1,F),Closure(t,Tuple_compute(a,b::c))::stk) -> executeK (Closure(t,b)) (Closure(t,Tuple_compute(a@[F],c))::stk)

| (Closure(t1,Const n),Closure(t,Tuple_compute(a,[]))::stk) -> executeK (Closure(t,Tuple (a@[Const n]))) stk
| (Closure(t1,T),Closure(t,Tuple_compute(a,[]))::stk) -> executeK (Closure(t,Tuple (a@[T]))) stk
| (Closure(t1,F),Closure(t,Tuple_compute(a,[]))::stk) -> executeK (Closure(t,Tuple (a@[F]))) stk

| (Closure(t,Var x),stk) -> executeK (value x t) stk

| (Closure(t,Lambda(x,e)),c::stk) -> executeK (Closure((fun x->c)::t,e)) stk 

| (Closure(t,App(e1,e2)),stk) -> executeK (Closure(t,e1)) (Closure(t,e2)::stk);;

let rec unpack a = match a with
Closure(t,Const n) -> Num n
| Closure(t,T) -> Bool true
| Closure(t,F) -> Bool false
| Closure(t,Tuple l) -> Tuple_answer (map (fun x-> unpack (Closure(t,x))) l);;