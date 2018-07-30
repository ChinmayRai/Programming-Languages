open List;; 
type symbol = string*int;;
type signat = symbol list;;     (*We define signature: signat to be a list of string*int where string is the name of constructor and int is its arity*)
type term = Var of string | Const of string | N of symbol*term list | FAIL | CUT;;
(*type atom = N of symbol*term list;;*)
(*type goal = atom list;;*)
type exp = Nat of int | Plus of exp*exp | Minus of exp*exp | Multiply of exp*exp | Div of exp*exp | Eq of exp*exp | Lt of exp*exp | Gt of exp*exp | Power of exp*exp ;;
type clause = Fact of term | Rule of term*term list | Arith of exp;;    (*The Fact contains one atom for head.The Rule contains an atom for head and an atom list for body*)
type program = clause list;;


let name a = match a with                  (*gives the name of constructor*)
(c,d) -> c;;
let arity a = match a with                  (*gives the arity of constructor*)
(c,d) -> d;;


let rec isin s l = match l with              (*returns true if symbol s is in symbol list l else false*)
[] -> false
| a::b -> if (name s)=(name a) then true
        else isin s b;;


let rec check_sig s = match s with              
[] ->true
| a::b -> if (isin a b)=true then false       (*this is clause checks if more than one symbol have same name *)
          else if (arity a)<0 then false       (*this clause checks if any symbo has negative arity *)
          else check_sig b;;


let and_1 a b = a && b;;   (*This is the usual and function, only difference being that it takes arguments post the operator *)

let rec wfterm t = match t with (*Verifies that all nodes have symbol names which are defined in signa and each node have no. of children=arity of symbol associated with it.*)
Var f -> (not(f=""))
| Const s -> true
| FAIL -> true
| CUT -> true
| N(a,b) -> if (length b) != (arity a) then false    
                else fold_left and_1 true (map wfterm b);;


let max a b = if a>b then a else b;; (*returns max of a and b*)

let rec height t = match t with   (*gives height of term defines according to signature s*)
Var k-> 0
| Const s -> 0
| FAIL -> 0
| CUT -> 0
| N (a,b) -> if (arity a)=0 then 0
				else if (arity a)=1 then 1+(height (hd b))
				else 1+ fold_left max 0 (map height b);;


let rec size t = match t with       (*gives size of term defines according to signature s*)
Var k -> 1
| Const s -> 1
| FAIL -> 1
| CUT -> 1
| N(a,b) -> 1 + fold_left (+) 0 (map size b);;


let rec union f l = match l with
[] -> []                                                                                    
| a::b -> (f a)@(union f b);;

let rec remove_duplicates a l = match l with	(*checks for duplicate elements*)
[] -> a
| x::xs -> if (mem x a) then remove_duplicates a xs
		   else remove_duplicates (a@[x]) xs;;

let rec vars t = match t with
Var r -> [Var r]
| Const s -> []
| FAIL -> []
| CUT -> []
| N(a,b) -> remove_duplicates [] (union vars b);;


let sub = [(Var ("x"),N(("g",1),[Var "y"]));Var ("y"),N(("f",2),[Var "z";Var "p"])];;

let rec subs sub term = match term with
 N(a,b) -> N(a,List.map (subs sub) b)
| Const s -> Const s
| FAIL -> FAIL
| CUT -> CUT
| Var p -> try List.assoc (Var p) sub with Not_found -> Var p ;;


let rec subst subl term = match subl with  (*List of Multiple subtitution represents compostion of substitution, with first element in the list applied first followed by application of second and so on.*)
[] -> term                                  (*Empty list represents identity substitution.*)
| a::b -> subst b (subs a term);;


exception NOT_UNIFIABLE;;          (*This exception is raised when two terms are not unifiable*)

let rec mgu t u = match (t,u) with    (*This function gives the most general unifier*)
(Var s,Var p) -> if s=p then [] else [[(Var s,Var p)]]				(*[ fun q ->  if q = Var s then Var p else q]*) 

| (Var s,Const p) -> [[(Var s,Const p)]]

| (Const p,Var s) -> [[(Var s,Const p)]]

| (Const p,Const s) -> if p=s then [] else raise NOT_UNIFIABLE

| (Const p,N(a,b)) -> raise NOT_UNIFIABLE

| ((FAIL|CUT), _) -> raise NOT_UNIFIABLE

| (_,(FAIL|CUT)) -> raise NOT_UNIFIABLE

| (N(a,b),Const p) -> raise NOT_UNIFIABLE

| (Var s,N(a,b)) -> if ((arity a)=0) then [[(Var s,N(a,b))]]
					else if (not(mem (Var s) (vars (N(a,b))))) then [[(Var s,N(a,b))]]							(*[ fun q -> if q=Var s then N(a,b) else q]*)
					else raise NOT_UNIFIABLE

| (N(a,b),Var s) -> if ((arity a)=0) then [[(Var s,N(a,b))]]
					else if (not(mem (Var s) (vars (N(a,b))))) then [[(Var s,N(a,b))]]							(*[ fun q -> if q=Var s then N(a,b) else q]*)
					else raise NOT_UNIFIABLE

| (N(a,[]),N(b,[])) -> if name a=name b then []
					   else raise NOT_UNIFIABLE

| (N(a,b),N(c,d)) -> if ((name a=name c)&&(List.length b=List.length d)) then
						let m = ref [] in
				     	    for i=1 to (arity a) do
						   		m := !m @ (mgu (subst !m (nth b (i-1))) (subst !m (nth d (i-1))))
						    done;
						!m
					 else raise NOT_UNIFIABLE;;



let rec check_query q = wfterm q;;

let check_clause a = match a with
Fact (a) -> wfterm a
| Rule (a,b) -> (wfterm a) && (fold_left and_1 true (map wfterm b));;

let rec check_program p = match p with
[] -> true
| l -> fold_left and_1 true (map check_clause l);;


let prog = [Fact(N(("likes",2),[N(("chinmay",0),[]);N(("food",0),[])]));Fact(N(("likes",2),[N(("chinmay",0),[]);N(("drinks",0),[])]));Fact(N(("Good",1),[N(("juice",0),[])]));Fact(N(("healthy",1),[N(("fruits",0),[])]));Fact(N(("healthy",1),[N(("food",0),[])]));Rule(N(("Good",1),[Var "x"]),[N(("healthy",1),[Var "x"]);N(("likes",2),[N(("chinmay",0),[]);Var "x"])])];;


let rec modify_prog program = match program with
  [] -> []
| hd :: tl -> (modify_clause hd)::(modify_prog tl)
and modify_clause x = match x with
| Fact (a) -> Fact (modify_term a)
| Rule (a,b) -> Rule ((modify_term a),(List.map modify_term b))
and modify_term x = match x with
| Var s -> Var ("~"^s)
| Const s -> Const s
| FAIL -> FAIL
| CUT -> CUT
| N (a,b) -> N(a,(List.map modify_term b))
;;


let rec printterm a = 
match a with
 Var x ->  Printf.printf "Var %s  " x
| N((a,0),[]) -> Printf.printf "%s/0 " a
| Const n -> Printf.printf "%s " n 
| FAIL -> Printf.printf "FAIL "
| CUT -> Printf.printf "CUT "
| N((a,b),l)  -> Printf.printf "%s/" a; Printf.printf "%d( " b; (map printterm l); Printf.printf ")";;

let print_clause p = match p with
Fact a -> printterm a;Printf.printf "."
| Rule(a,b) -> printterm a;Printf.printf ":-";for i=0 to (List.length b)-1 do printterm (List.nth b i); Printf.printf "," done;;


let rec print_prog p = match p with
[] -> Printf.printf ""
| a::b -> print_clause a;Printf.printf "\n"; print_prog b;;

let prnt_sub a = 
if List.length a=0 then Printf.printf "[]"
else if List.length a=1 then  match List.hd a with (x,y) -> Printf.printf "[(";printterm x;Printf.printf ",";printterm y;Printf.printf ")]"
else match List.hd a with (x,y) -> Printf.printf "[(";printterm x;Printf.printf ",";printterm y;Printf.printf ")";
			for i=1 to (List.length a)-1 do
				match (List.nth a i) with (x,y) ->  Printf.printf ";(";printterm x;Printf.printf ",";printterm y;Printf.printf ")";
			done;
			Printf.printf "]";;

let subst_print l =
if List.length l=0 then Printf.printf "[]" 
else Printf.printf "["; prnt_sub (List.hd l);
		if (List.length l)>1 then
		for i=1 to (List.length l)-1 do
			 Printf.printf ";";prnt_sub (List.nth l i);
		done;
Printf.printf "]\n";;

let rec print_answer a = match a with
[] -> Printf.printf "\n"
| (c,d)::b -> Printf.printf "\n"; printterm c ; Printf.printf "=" ; printterm d; Printf.printf "\n"; print_answer b;;

let get_new_goal clause unif old_goal = match clause with
Fact a -> (map (subst unif)  old_goal) 
| Rule (a,b) -> (map (subst unif) (b@old_goal));;

let rec remove_id sub = match sub with
[] ->[]
| a::b -> if a = [] then (remove_id b) else (a::(remove_id b)) ;;

let rec break_down sub ans = match (sub,ans) with
([],ans) -> ans
| (a::b,ans) ->  break_down b (ans@a);;

let rec rm c l = match l with
[] -> []
| a::b -> if a=c then b else a::(rm c b);;
 

let rctf sub =
	let s= ref sub in
for i=0 to (List.length sub)-1 do
	let k = List.nth sub i  in
	try
	let a = List.assoc (snd k) (!s) in s := (fst k,a)::(rm (snd k,a) (rm k (!s)))
	with Not_found -> ()
done;
!s;;

let rectify subst = rctf (break_down (remove_id subst) []) ;;

let print_unify head = Printf.printf "Trying to unify with:";(printterm head); Printf.printf "\n";;

exception BREAK;;


let rec eval_query2 (q,prog,sub) = match q with
[] -> let s=read_line() in 
	  if s=";" then print_answer(rectify(sub)) else if s="." then raise Exit;
| q1::qs ->	try
			if q1=FAIL then () else
				for i=0 to (List.length prog)-1 do
					let head = match (List.nth prog i) with Fact a -> a | Rule (a,b) -> a in
					let unif = try (mgu q1 head) with NOT_UNIFIABLE -> [[(FAIL,FAIL)]] in
					if unif <> [[(FAIL,FAIL)]] then  
						let q'=(get_new_goal (List.nth prog i) unif qs) in eval_query2(q',modify_prog prog,(unif@sub));
					if qs<>[] then	
					if (List.hd qs)=CUT then raise BREAK
				done;
			with BREAK -> ();;


let rec eval_query1 (q,prog,sub) = match q with
[] -> Printf.printf "true\n"; raise Exit
| q1::qs ->	if q1=FAIL then () else
			for i=0 to (List.length prog)-1 do
				let head = match (List.nth prog i) with Fact a -> a | Rule (a,b) -> a in
				let unif = try (mgu q1 head) with NOT_UNIFIABLE -> [[(FAIL,FAIL)]] in
				if head = q1 then eval_query1(qs,prog,sub)
				else if unif <> [[(FAIL,FAIL)]] then eval_query1((get_new_goal (List.nth prog i) unif qs),modify_prog prog,(unif@sub));
		   done;;

let eval_query (q,prog,sub) = try (if (vars (List.hd q))=[] then eval_query1(q,modify_prog prog,sub) else eval_query2(q,modify_prog prog,sub))
								with Exit -> ();;


let rec print_term a = 
match a with
 Var x ->  Printf.printf "Var %s " x
| N((a,0),[]) -> Printf.printf "N((%s,0),[])" a
| Const n -> Printf.printf "Const %s " n 
| FAIL -> Printf.printf "FAIL "
| CUT -> Printf.printf "CUT "
| N((a,b),l)  -> Printf.printf "N((%s," a; Printf.printf "%d),[ " b;
 print_term (hd l);
for i=1 to length l -1 do
	Printf.printf ";";(print_term (nth l i));
done;
  Printf.printf "])";;

let rec print_arith a = match a with
Nat n -> Printf.printf "Nat(%d)" n
| Plus(e1,e2) -> Printf.printf "Plus(";print_arith e1;Printf.printf",";print_arith e2;Printf.printf")"
| Minus(e1,e2) -> Printf.printf "Minus(";print_arith e1;Printf.printf",";print_arith e2;Printf.printf")"
| Multiply(e1,e2) -> Printf.printf "Multiply(";print_arith e1;Printf.printf",";print_arith e2;Printf.printf")"
| Div(e1,e2) -> Printf.printf "Div(";print_arith e1;Printf.printf",";print_arith e2;Printf.printf")"
| Power(e1,e2) -> Printf.printf "Power(";print_arith e1;Printf.printf",";print_arith e2;Printf.printf")"
| Eq(e1,e2) -> Printf.printf "Eq(";print_arith e1;Printf.printf",";print_arith e2;Printf.printf")"
| Gt(e1,e2) -> Printf.printf "Gt(";print_arith e1;Printf.printf",";print_arith e2;Printf.printf")"
| Lt(e1,e2) -> Printf.printf "Lt(";print_arith e1;Printf.printf",";print_arith e2;Printf.printf")";;


let print_tree a = match a with
Fact a  -> Printf.printf "Fact(";print_term a;Printf.printf ")\n"
| Arith a -> Printf.printf "Arith(";print_arith a;Printf.printf ")\n"
| Rule(a,b) -> Printf.printf "Rule(";print_term a;Printf.printf ",[";
print_term (hd b);
for i=1 to length b -1 do
	Printf.printf ";";(print_term (nth b i));
done;
Printf.printf "])\n";;
 

let rec print_ast a = match a with
[] -> ()
| b::c -> print_tree b;print_ast c;;

