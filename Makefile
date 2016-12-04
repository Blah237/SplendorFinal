main:
	ocamlbuild -pkgs oUnit main.byte
	
test:
	ocamlbuild -pkgs oUnit,str,unix test.byte && ./test.byte

clean:
	ocamlbuild -clean
	
zip:
	zip a4src.zip *.ml{,i,y,l}