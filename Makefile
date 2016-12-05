main:
	ocamlbuild -pkgs oUnit,graphics main.byte && ./main.byte

clean:
	ocamlbuild -clean