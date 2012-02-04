all: build

OCAMLBUILD=ocamlbuild -use-ocamlfind
FILE=main.native

build:
	$(OCAMLBUILD) $(FILE)

run:
	$(OCAMLBUILD) $(FILE) --

clean:
	$(OCAMLBUILD) -clean

package:
	zip impl.zip *.ml* _tags Makefile
