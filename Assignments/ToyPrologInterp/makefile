make:
	ocamlc -c ast.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o main.exe ast.cmo lexer.cmo parser.cmo main.cmo
	ocamlmktop -o aa.top ast.cmo lexer.cmo parser.cmo main.cmo
clean:
	rm *.exe *.cmo *.cmi *.mli lexer.ml parser.ml