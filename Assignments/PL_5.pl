isBool(true).
isBool(false).
/**********************************************************************************************/
append([],X,X).
append([X|Xs],L2,[X|L3]) :- append(Xs,L2,L3).
prefix(A,B) :- append(A,W,B).
suffix(A,B) :- append(W,A,B).

substring(A,B) :- prefix(A,C),suffix(C,B).
 equal_list_length([],[]) 			:- !.
equal_list_length([A|As],[B|Bs]) 	:- equal_list_length(As,Bs). 
/**********************************************************************************************/
/*var ka type TypeVar hoga ya uske typeassumption ka answer hoga?*/
vars([],[]).
vars([(var(X),Z)|Gs],[var(X)|L]):-vars(Gs,L). 

member(X,[])		:- fail.
member(X,[X|Xs]) 	:- !.
member(X,[Y|Xs]) 	:- member(X,Xs).

lookup(Gamma,var(X),typevar(T)) :- member((var(X),typevar(T)),Gamma).

hastype(Gamma,var(X),T)				:- lookup(Gamma,var(X),typevar(T)).
hastype(Gamma,const(X),intT) 		:- integer(X).
hastype(Gamma,X,boolT) 				:- isBool(X).
hastype(Gamma,plus(A,B),intT) 		:- hastype(Gamma,A,intT),hastype(Gamma,B,intT).
hastype(Gamma,minus(A,B),intT) 		:- hastype(Gamma,A,intT),hastype(Gamma,B,intT).
hastype(Gamma,multiply(A,B),intT)	:- hastype(Gamma,A,intT),hastype(Gamma,B,intT).
hastype(Gamma,div(A,B),intT) 		:- hastype(Gamma,A,intT),hastype(Gamma,B,intT).
hastype(Gamma,power(A,B),intT) 		:- hastype(Gamma,A,intT),hastype(Gamma,B,intT).
hastype(Gamma,mod(A,B),intT) 		:- hastype(Gamma,A,intT),hastype(Gamma,B,intT).
hastype(Gamma,abs(A),intT) 			:- hastype(Gamma,A,intT).
hastype(Gamma,neg(A),intT) 			:- hastype(Gamma,A,intT).
hastype(Gamma,lt(A,B),boolT) 		:- hastype(Gamma,A,intT),hastype(Gamma,B,intT).
hastype(Gamma,gt(A,B),boolT) 		:- hastype(Gamma,A,intT),hastype(Gamma,B,intT).
hastype(Gamma,le(A,B),boolT) 		:- hastype(Gamma,A,intT),hastype(Gamma,B,intT).
hastype(Gamma,ge(A,B),boolT) 		:- hastype(Gamma,A,intT),hastype(Gamma,B,intT).
hastype(Gamma,and(A,B),boolT) 		:- hastype(Gamma,A,boolT),hastype(Gamma,B,boolT).
hastype(Gamma,or(A,B),boolT) 		:- hastype(Gamma,A,boolT),hastype(Gamma,B,boolT).
hastype(Gamma,implies(A,B),boolT) 	:- hastype(Gamma,A,boolT),hastype(Gamma,B,boolT).
hastype(Gamma,xor(A,B),boolT)		:- hastype(Gamma,A,boolT),hastype(Gamma,B,boolT).
hastype(Gamma,not(A),boolT) 		:- hastype(Gamma,A,boolT).
hastype(Gamma,eq(A,B),boolT)		:- hastype(Gamma,A,T),hastype(Gamma,B,T). /* hastype(Gamma,A,X),hastype(Gamma,B,X),A=:=B.*/
hastype(Gamma,if_then_else(A,B,C),X):- hastype(Gamma,A,boolT),hastype(Gamma,B,X),hastype(Gamma,C,X).
hastype(Gamma,tuple([]),cartesian([])) :- !.
hastype(Gamma,tuple([A|As]),cartesian([T|Ts])) :- equal_list_length(As,Ts),hastype(Gamma,A,T),hastype(Gamma,tuple(As),cartesian(Ts)). 
hastype(Gamma,tuple([]),Y) 			:- !.
hastype(Gamma,tuple([A|As]),Y)		:- hastype(Gamma,A,Y),hastype(Gamma,tuple(As),Y).
hastype(Gamma,absrt(var(X),E),arrow(T1,T2)) :- hastype([(var(X),typevar(T1))|Gamma],E,T2).
hastype(Gamma,app(E1,E2),T2) 		:- hastype(Gamma,E1,arrow(T1,T2)),hastype(Gamma,E2,T1).
hastype(Gamma,proj(A,E),Y)			:- integer(A),hastype(Gamma,E,Y).							/*A ki range bhi check krni hai kya?*/
hastype(Gamma,let_in_nd(D,E),T)		:- typeElaborates(Gamma,D,Gamma1),append(Gamma1,Gamma,Gamma2),hastype(Gamma2,E,T).


typeElaborates(Gamma,def(var(X),E),[(var(X),typevar(T))])	:- hastype(Gamma,E,T).            
typeElaborates(Gamma,seq(E1,E2),Gamma3) 			:- typeElaborates(Gamma,E1,Gamma1),append(Gamma1,Gamma,Gamma4),typeElaborates(Gamma4,E2,Gamma2),append(Gamma2,Gamma1,Gamma3).
typeElaborates(Gamma,par(E1,E2),Gamma3) 			:- typeElaborates(Gamma,E1,Gamma1),typeElaborates(Gamma,E2,Gamma2),vars(Gamma1,L1),vars(Gamma2,L2),intersection(L1,L2,[]),append(Gamma1,Gamma2,Gamma3).
typeElaborates(Gamma,lcl_in_nd(E1,E2),Gamma2)  		:- typeElaborates(Gamma,E1,Gamma1),append(Gamma1,Gamma,Gamma4),typeElaborates(Gamma4,E2,Gamma2).
	


/*
?  hastype([],const(3),intT).
true .

?- hastype([],const(a),intT).
false.

?- hastype([],const(a),boolT).
false.

?- hastype([],true,boolT).
true.

?- hastype([],false,boolT).
true.

?- hastype([],plus(const(2),const(3)),intT).
true .

?- hastype([],plus(const(2),true),intT).
false.

hastype([],minus(const(2),true),intT).
false.

hastype([],minus(const(2),const(3)),intT).
true.

?- hastype([],multiply(const(2),const(3)),intT).
true .

?- hastype([],div(const(2),const(3)),intT).
true .

?- hastype([],mod(const(2),const(3)),intT).
true .

?- hastype([],power(const(2),const(3)),intT).
true.

?- hastype([],abs(true),intT).
false.

?- hastype([],abs(const(5)),intT).
true.

?- hastype([],neg(true),intT).
false.

?- hastype([],neg(4),intT).
false.

?- hastype([],neg(const(3)),intT).
true.

?- hastype([],lt(const(3),const(5)),boolT).
true .

?- hastype([],lt(const(7),const(5)),boolT).
true .

?- hastype([],gt(const(4),const(6)),booT).
false.

?- hastype([],gt(const(4),const(6)),boolT).
true .

?- hastype([],le(const(5),const(6)),boolT).
true .

?- hastype([],ge(const(5),const(5)),boolT).
true .

?- hastype([],and(true,false),boolT).
true.

?- hastype([],implies(true,false),boolT).
true.

?- hastype([],xor(true,false),boolT).
true.

?- hastype([],or(true,false),boolT).
true.

?-  hastype([],not(true),boolT).
true .

?- hastype([],eq(and(true,false),plus(const(2),const(3))),boolT).
false.

?- hastype([],eq(and(true,false),or(true,false)),boolT).
true.

?- hastype([],eq(const(4),const(5)),boolT).
true.

?- hastype([],if_then_else(true,const(5),const(6)),boolT).
false.

?- hastype([],if_then_else(true,const(5),const(6)),intT).
true.

?- hastype([],tuple([const(3),false,const(5)]),cartesian([intT,boolT,intT])).
true .

?- hastype([],absrt(var(x),plus(var(x),const(5))),arrow(intT,intT)).
true .

?- hastype([],absrt(var(x),plus(var(x),const(5))),arrow(boolT,intT)).
false.

?- hastype([],app(absrt(var(x),plus(var(x),const(5))),true),intT).
false.

?- hastype([],app(absrt(var(x),plus(var(x),const(5))),const(0)),intT).
true

?- hastype([],proj(1,tuple([false,true,const(3)])),intT).
false.

?- hastype([],proj(1,tuple([false,true,const(3)])),boolT).
false.

?- hastype([],proj(1,tuple([false,true,true])),boolT).
true.

?- hastype([],proj(1,tuple([const(4),const(4),const(4)])),intT).
true 

?- hastype([],let_in_nd(def(var(x),const(2)),multiply(const(4),var(x))),intT).
true .

?- hastype([],let_in_nd(def(var(x),true),or(false,var(x))),boolT).
true .

?- typeElaborates([],def(var(x),plus(const(2),true)),[(var(x),typevar(intT))]).
false.

?- typeElaborates([],def(var(x),plus(const(2),const(6))),[(var(x),typevar(intT))]).
true 

?- typeElaborates([],seq(def(var(x),const(2)),def(var(y),plus(const(3),const(4)))),[(var(y),typevar(intT)),(var(x),typevar(intT))]).
true .

?- typeElaborates([],seq(def(var(x),const(2)),def(var(y),plus(const(3),var(x)))),[(var(y),typevar(intT)),(var(x),typevar(intT))]).
true .

?- typeElaborates([],par(def(var(x),const(2)),def(var(y),plus(const(3),var(x)))),[(var(y),typevar(intT)),(var(x),typevar(intT))]).
false.

?- typeElaborates([],par(def(var(x),const(2)),def(var(y),plus(const(3),const(5)))),[(var(x),typevar(intT)),(var(y),typevar(intT))]).
true .

?- typeElaborates([],lcl_in_nd(def(var(x),const(2)),def(var(y),plus(const(3),var(x)))),[(var(y),typevar(intT))]).
true .

?- typeElaborates([],seq(def(var(x),const(2)),def(var(y),var(x))),[(var(y),typevar(intT)),(var(x),typevar(intT))]).
true .

?- typeElaborates([],par(def(var(x),const(2)),def(var(y),var(x))),[(var(x),typevar(intT)),(var(y),typevar(intT))]).
false.

?- typeElaborates([],par(def(var(x),const(2)),def(var(y),const(3))),[(var(x),typevar(intT)),(var(y),typevar(intT))]).
true 
*/