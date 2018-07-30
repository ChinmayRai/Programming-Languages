let sig1 = [("X",0);("Y",0);("f",1);("g",2);("h",3);("*",2)];;
let sig2 = [("X",0);("Y",0);("Z",0);("f",1);("g",2);("f",3);("*",2)];;
let sig3 = [("f",1)];;
let sig4 = [("X",0);("Y",0);("Z",0)];;

let term1 = (Node (("f",1),[Var "X"]));;
let term2 = (Node (("g",2),[Var "X";Node(("h",3),[Node(("f",1 ),[Var "X"]);Var "Y"])]));;
let term3 = (Node (("g",2),[Var "X";Node(("*",2),[Var "Y";Node (("*",2),[Var "X";Var "Y"])])]));;
let term4 = (Node (("g",2),[Var "X";Node(("*",2),[Var "Y";Var "X"])]));;
let term5 = (Node (("g",2),[Var "Z";Node(("*",2),[Var "X";Var "Z"])]));;
let term6 = (Node (("g",2),[Var "Z";Node(("g",2),[Var "X";Var "Z"])]));;
let term7 = (Var "X");;
let term8 = (Node (("K",0),[]));;
let term9 = (Node (("X",0),[]));;
let term10 = (Node (("g",2),[Var "X";Node(("h",3),[Node(("f",1),[Var "X"]);Var "Y";Node (("X",0),[])])]));;
let term11 = (Node (("g",2),[Var "X";Node(("h",3),[Node(("f",1),[Var "X"]);Var "Y";Node (("f",1),[Var "X"])])]));;
let term12 = (Node (("g",2),[Var "Z";Node(("*",2),[Var "Z";Node (("*",2),[Var "X";Var "Y"])])]));;
let term13 = (Node (("$",2),[Var "P";Var"Q"]));;
let term14 = (Node (("$",2),[Node (("2",0),[]); Node (("4",0),[])]));;
let term15 = (Node (("$",2),[Node (("2",0),[]); Node (("3",0),[])]));;

Printf.printf "(1)check_sig sig1 : %B\n" (check_sig sig1);;
Printf.printf "(2)check_sig sig2 : %B\n" (check_sig sig2);;
Printf.printf "(3)check_sig sig3 : %B\n" (check_sig sig3);;
Printf.printf "(4)check_sig sig4 : %B\n\n" (check_sig sig4);;

Printf.printf "(5)wfterm term1 sig1 : %B\n" (wfterm sig1 term1 );;
Printf.printf "(6)wfterm term2 sig1 : %B\n" (wfterm sig1 term2 );;
Printf.printf "(7)wfterm term7 sig4 : %B\n" (wfterm sig4 term7 );;
Printf.printf "(8)wfterm term8 sig4 : %B\n" (wfterm sig4 term8 );;
Printf.printf "(9)wfterm term9 sig4 : %B\n\n" (wfterm sig4 term9 );;

Printf.printf "(10)ht term9 : %d\n" (height sig1 term9);;
Printf.printf "(11)ht term7 : %d\n" (height sig1 term7);;
Printf.printf "(12)ht term4 : %d\n" (height sig1 term4);;
Printf.printf "(13)ht term10 : %d\n" (height sig1 term10);;
Printf.printf "(14)ht term11 : %d\n\n" (height sig1 term11);;

Printf.printf "(15)size term9 : %d\n" (size sig1 term9);;
Printf.printf "(16)size term7 : %d\n" (size sig1 term7);;
Printf.printf "(17)size term4 : %d\n" (size sig1 term4);;
Printf.printf "(18)size term10 : %d\n" (size sig1 term10);;
Printf.printf "(19)size term11 : %d\n\n" (size sig1 term11);;

Printf.printf "(20)vars term9 : ";; (vars sig1 term9);; Printf.printf("\n");;
Printf.printf "(21)vars term7 : ";; (vars sig1 term7);; Printf.printf("\n");;
Printf.printf "(22)vars term4 : ";; (vars sig1 term4);; Printf.printf("\n");;
Printf.printf "(23)vars term10 : ";; (vars sig1 term10);; Printf.printf("\n");;
Printf.printf "(24)vars term11 : ";; (vars sig1 term11);; Printf.printf("\n\n");;


	
Printf.printf "(33)mgu term3  term12 : ";; ((mgu sig1 term3 term12));; Printf.printf("\n");;
Printf.printf "(34)mgu term12 term3  : ";; ((mgu sig1 term12 term3));; Printf.printf("\n\n");;

Printf.printf "(33.1)subst term12 (mgu term3 term12)  : ";; (subst (mgu sig1 term3 term12) term12);; Printf.printf("\n");;
Printf.printf "(33.2)subst term3  (mgu term3 term12)  : ";; (subst (mgu sig1 term3 term12) term3);; Printf.printf("\n\n");;

Printf.printf "(34.1)subst term12 (mgu term12 term3)  : ";; (subst (mgu sig1 term12 term3) term12);; Printf.printf("\n");;
Printf.printf "(34.2)subst term3  (mgu term12 term3)  : ";; (subst (mgu sig1 term12 term3) term3);; Printf.printf("\n\n");;


let sig11=[("+",2);("1",0);("-",2)];;

let term = Node(("+",2), [Var "x"; Node(("1",0), [])]);; (* x + 1 *)
let term2 = Node(("+",2), [Var "x"; Node(("+",2), [Node(("1",0), [])])]);; (* invalid *)
let term3 = Node(("+",2), [Node(("+",2), [Var "x";Var "y"]); Node(("-",2), [Var "x";Var "y"])]);;
(* (x + y) + (x - y) *)
let term4 = Var "z";;
let term5 = Var "y";;

mgu sig11 term term3;;
(* Exception: NOT_UNIFIABLE *)
mgu sig11 term term4;;
(* Exception: NOT_UNIFIABLE. *)
mgu sig11 term3 term5;;
(* Exception: NOT_UNIFIABLE. *)







