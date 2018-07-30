	open String;;
	type a_star = Empty | A of char | App of a_star * a_star;;
	exception Empty_string;;

	type str = C of a_star*int*int;;

	let lngth(C (a,b,c)) = c;;

	let mrk (C (a,b,c)) = b;;

	let strng (C (a,b,c)) =a;;


	let rec to_a_star a =
	  let n= length a in
	if n=0  then Empty
	else if n=1 then A (a.[0])
	else App(A (a.[0]),to_a_star(sub a 1 (n-1)));;

	let create a = C (to_a_star a,0,length a);;

	exception Atlast;;
	exception Atfirst;;
	exception Tooshort;;

	let forward a =
	if mrk a < (lngth a) -1 then C(strng a,(mrk a)+1,lngth a)
	else raise Atlast;;

	let back a =
	if mrk a > 0 then C(strng a,(mrk a)-1,lngth a)
	else raise Atfirst;;

	let moveTo a n =
	if n < lngth a && n>=0 then C(strng a, n,lngth a)
	else raise Tooshort;;

	let rec lgh a = match a with 
	Empty -> 0
	| A a -> 1
	| App (a,b) -> lgh a + lgh b;;

	let rec rplc w a n = match a with
	Empty -> raise Empty_string
	| A a -> A w
	| App(a,b) -> if lgh a>n then App(rplc w a n,b)
	              else App(a,rplc w b (n-(lgh a)));;

	let replace w s = rplc w (strng s) (mrk s);;

	let rec lgh a = lngth a;;

	let rec nonempty a = 
	if lngth a=0 then false
	else true;; 

	let concat a b = C(App (strng a,strng b),mrk a +mrk b,lngth a + lngth b);;

	let rec fst a = match a with
	Empty -> raise Empty_string
	| A b -> String.make 1 b
	| App (b,c) -> fst b;;

	let rec lst a = match a with
	Empty -> raise Empty_string
	| A b -> String.make 1 b
	| App (d,b) -> lst b;;

	let first a = fst (strng a);;
	let last a= lst (strng a);;

	let rec rvrs a = match a with
	Empty -> Empty
	| A s -> A s
	| App(a,b) -> App(rvrs b,rvrs a);;

	let reverse a = C(rvrs (strng a),mrk a,lngth a);;

	let rec eval e = match e with 	
	Empty -> ""
	| A a -> String.make 1 a
	| App (a,b) -> eval a ^ eval b;;

	let str_to_string a = eval (strng a);;