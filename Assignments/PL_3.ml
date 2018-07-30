type symbol = string*int;;                                       
type signat = symbol list;;     (*We define signature: signat to be a list of string*int where string is the name of constructor and int is its arity*)
open List;; 


type preterm = Var of string | Node of symbol*preterm list;;   (*preterm can either be a variable or treenode with a particular constructor & no. of children=arity of constructor*)


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


let rec get_arity a s = match s with  (*This fuction gives arity of symbol in signature s whose name is a; it retruns -1 if no such symbol exists in signature s*)
 [] -> -1
| c::d ->if a=(name c) then arity c
		else get_arity a d  ;;


let and_1 a b = a && b;;   (*This is the usual and function, only difference being that it takes arguments post the operator *)

let rec wfterm signa t = match t with (*Verifies that all nodes have symbol names which are defined in signa and each node have no. of children=arity of symbol associated with it.*)
Var f -> true
| Node(a,b) -> if (length b) != (get_arity (name a) signa) then false    
                else fold_left and_1 true (map (wfterm signa) b);;


let max a b = if a>b then a else b;; (*returns max of a and b*)

let rec height s t = match t with   (*gives height of term defines according to signature s*)
Var k-> 0
| Node (a,b) -> if (get_arity (name a) s)=0 then 0
				else if (get_arity (name a) s)=1 then 1+(height s (hd b))
				else 1+ fold_left max 0 (map (height s) b);;


let rec size s t = match t with       (*gives size of term defines according to signature s*)
Var k -> 1
| Node(a,b) -> 1 + fold_left (+) 0 (map (size s) b);;


let rec union f l = match l with
[] -> []                                                                                    (*check for duplicate elements*)
| a::b -> (f a)@(union f b);;

let rec remove_duplicates a l = match l with
[] -> a
| x::xs -> if (mem x a) then remove_duplicates a xs
		   else remove_duplicates (a@[x]) xs;;

let rec vars signa t = match t with
Var r -> [Var r]
| Node(a,b) -> remove_duplicates [] (union (vars signa) b);;

let sub a = match a with                (*A subtitution a list of function(s):preterm->preterm .A list with single element represents one subtitution. *)
Var "x" -> Node(("g",1),[Var "y"])
| Var "y" -> Node(("f",2),[Var "z";Var "p"])
| Var s -> Var s;;
 

let rec subs sub term = match term with
Var p -> sub (Var p)
| Node(a,b) -> Node(a,map (subs sub) b);;

let rec subst subl term = match subl with  (*List of Multiple subtitution represents compostion of substitution, with first element in the list applied first followed by application of second and so on.*)
[] -> term                                  (*Empty list represents identity substitution.*)
| a::b -> subst b (subs a term);;


exception NOT_UNIFIABLE;;          (*This exception is raised when two terms are not unifiable*)

let rec mgu signa t u = match (t,u) with    (*This function gives the most general unifier*)
(Var s,Var p) -> if s=p then [] 
				 else [ fun q ->  if q = Var s then Var p else q] 

| (Var s,Node(a,b)) -> if ((get_arity (name a) signa)=0) then [ fun q -> if q=Var s then Node(a,b) else q]
					   else if (not(mem (Var s) (vars signa (Node(a,b))))) then [ fun q -> if q=Var s then Node(a,b) else q]
					   else raise NOT_UNIFIABLE

| (Node(a,b),Var s) -> if ((get_arity (name a) signa)=0) then [ fun q -> if q=Var s then Node(a,b) else q]
					   else if (not(mem (Var s) (vars signa (Node(a,b))))) then [ fun q -> if q=Var s then Node(a,b) else q]
					   else raise NOT_UNIFIABLE

| (Node(a,[]),Node(b,[])) -> if name a=name b then []
								 else raise NOT_UNIFIABLE

| (Node(a,b),Node(c,d)) -> if name a=name c then
						     let m = ref [] in
				     	     for i=1 to (get_arity (name a) signa) do
						   		 m := !m @ (mgu signa (subst !m (nth b (i-1))) (subst !m (nth d (i-1))))
						     done;
						     !m
						   else raise NOT_UNIFIABLE;;


let signa1 = [("f",2);("r",2);("r",1)];;
check_sig signa1;;

let signa2 = [("f",2);("r",2);("p",-1)];;
check_sig signa2;;


let signa = [("e",2);("f", 2);("i",1);("g", 1); ("k", 0);("h",0)];;
check_sig signa;;

name ("g",2);;

arity ("g",2);;

get_arity "f" signa;;

get_arity "h" signa;;

isin ("g",1) signa;;


wfterm signa (Node(("f",2),[Node(("g",1),[Var "x"]);Var "z"]));;

wfterm signa (Node(("f",2),[Node(("g",1),[Var "x";Var "i"]);Var "z"]));;


let t = (Node(("f",2),[Node(("f",2),[Node(("f",2),[Var "g";Var "k"]);Node(("h",0),[])]);Node(("f",2),[Var "g";Var "k"])]));;


height signa t;;


size signa t;;

vars signa t;;


subst (mgu signa (Var "x") (Var "x")) (Var "x");;

subst (mgu signa (Var "x") (Var "x")) (Var "y");;

subst (mgu signa (Var "x") (Var "y")) (Var "x");;

subst (mgu signa (Var "x") (Var "y")) (Var "y");;

subst (mgu signa (Var "x") (Var "y")) (Var "t");;

subst (mgu signa (Var "x") (Node(("k",0),[]))) (Var "x");;

subst (mgu signa (Var "x") (Node(("k",0),[]))) (Var "y");;




subst (mgu signa (Var "x") (Node(("k",0),[]))) (Var "x");;

subst (mgu signa (Var "x") (Node(("k",0),[]))) (Var "y");;

subst (mgu signa (Var "x") (Node(("f",2),[Var "f";Var "k"]))) (Var "x");;

subst (mgu signa (Var "x") (Node(("f",2),[Var "f";Var "k"]))) (Var "y");;

subst (mgu signa (Var "x") (Node(("f",2),[Var "f";Var "k"]))) (Var "f");;

(mgu signa (Var "x") (Node(("f",2),[Var "x";Var "k"])));;




subst (mgu signa ( Node(("k",0),[]) ) ( Node(("k",0),[]) ) )  (Node(("k",0),[]));;

subst (mgu signa ( Node(("k",0),[]) ) ( Node(("k",0),[]) ) )  (Node(("h",0),[]));;


subst (mgu signa ( Node(("k",0),[]) ) ( Node(("h",0),[]) ) )  (Node(("k",0),[]));;


(mgu signa ( Node(("h",0),[]) ) ( Node(("g",1),[Var "p"]) ) );;


(mgu signa ( Node(("g",1),[Var "p"]) )  ( Node(("h",0),[]) ) );;


mgu signa (Node(("i",1),[Var "a"])) (Node(("g",1),[Var "r"]));;


mgu signa (Node(("f",2),[Var "x";Var "y"])) (Node(("f",2),[Node(("g",1),[Var "x"]);Var "z"]));;


mgu signa (Node(("f",2),[Var "x";Var "y"])) (Node(("f",2),[Node(("g",1),[Var "y"]);Var "z"]));;



let aa = mgu signa (Node(("f",2),[Var "x";Var "y"])) (Node(("f",2),[Node(("g",1),[Var "y"]);Var "z"]));;

subst aa (Node(("f",2),[Var "x";Var "y"]))=subst aa (Node(("f",2),[Node(("g",1),[Var "y"]);Var "z"]));;










