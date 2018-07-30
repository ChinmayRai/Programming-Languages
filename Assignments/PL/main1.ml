open PL_6



let ast = 
    let filename = Sys.argv.(1) in 
    let file = open_in filename in
    let lexbuf = Lexing.from_channel file in
    let rec make_ast acc = 
      let result = Parser.main Lexer.token lexbuf in
        match result with Fact(N(("File_end",0),[])) -> (List.rev acc)
        | _ -> (make_ast (result::acc)) 
      in 
    (make_ast [])
;;



(*print_prog ast;; *)

Printf.printf "\n\n";;
print_ast ast;;

Printf.printf "\n\n";;



let _ = 
  Printf.printf "?-"; flush stdout;
  let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.main Lexer.token lexbuf in
        match result with 
        Fact(N(("exit",0),[])) -> Printf.printf "EXITING\n";flush stdout; exit 0
        | Fact(a) -> eval_query([a],ast,[]);Printf.printf "\n?-"; flush stdout;
        | _-> Printf.printf "INVALID INPUT GOAL\n";Printf.printf "\n?-"; flush stdout;
    done
;;
  


