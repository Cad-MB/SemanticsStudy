LEX_ML = ocamllex
YACC_ML = /usr/bin/ocamlyacc
#YACC_ML = /usr/local/bin/ocamlyacc
OCAMLC = ocamlc

all: prologTerm evaluateur

prologTerm: parser prologTerm.ml
	$(OCAMLC) -o prologTerm ast.cmo lexer.cmo parser.cmo prologTerm.ml

evaluateur: parser evaluateur.ml
	$(OCAMLC) -o evaluateur ast.cmo lexer.cmo parser.cmo evaluateur.ml

parser: ast.ml lexer.mll parser.mly
	$(OCAMLC) -c ast.ml
	$(LEX_ML) -o lexer.ml lexer.mll
	$(YACC_ML) -b parser parser.mly
	$(OCAMLC) -c parser.mli
	$(OCAMLC) -c lexer.ml
	$(OCAMLC) -c parser.ml

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f prologTerm
	rm -f evaluateur
	rm -f lexer.ml
	rm -f parser.mli
	rm -f parser.ml
	rm *~
