make:
	ocamlc -c PL_6.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main1.ml
	ocamlc -o main1.exe PL_6.cmo  lexer.cmo parser.cmo main1.cmo

clean:
	rm *.exe *.cmo *.cmi *.mli lexer.ml parser.ml
