open Ast

let filename = Sys.argv.(1);;
Printf.printf "%s\n" filename;;

let file = open_in filename;;
let line = input_line file in  (* read line from in_channel and discard \n *)
    print_endline line;
    Printf.printf"\n";;

let tokenize s =
	let lb = Lexing.from_string s in
	let rec helper l =
		try
			let t = Lexer.token lb in
			if t = Parser.EOF then List.rev l else helper (t::l)
		with _ -> List.rev l
	in 
		helper [];;

let parse str =
	let lb = Lexing.from_string str
	in
        Parser.main (Lexer.token) lb;;

tokenize (input_line file);;

let rec printterm a = 
match a with
 Var x ->  Printf.printf "Var %s  " x
| N((a,0),[]) -> Printf.printf "%s/0 " a
| Const n -> Printf.printf "%s " n 
| FAIL -> Printf.printf "FAIL "
| CUT -> Printf.printf "CUT "
| N((a,b),l)  -> Printf.printf "%s/" a; Printf.printf "%d( " b; (List.map printterm l); Printf.printf ")";;

let print_clause p = match p with
Fact a -> printterm a;Printf.printf "."
| Rule(a,b) -> printterm a;Printf.printf ":-";for i=0 to (List.length b)-1 do printterm (List.nth b i); Printf.printf "," done;;

let rec print_prog p = match p with
[] -> Printf.printf ""
| a::b -> print_clause a;Printf.printf "\n"; print_prog b;;

print_prog [parse (input_line file)];;

Printf.printf "sbhdcdsc";;




(*

let table = 
    let filename = "ex.pl" in 
    Printf.printf "%s" filename;
    let file = open_in filename in
    Printf.printf "file id opened";
    let lexbuf = Lexing.from_channel file in
    let rec createTable acc = 
      let result = Parser.main Lexer.token lexbuf in
        match result with Fact(N(("file_end",0),[])) -> acc
        | _ -> (createTable (result::acc)) 
      in 
    (print_prog(createTable []));
    (createTable [])
;;

*)