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
	git archive --format zip --output impl.zip --prefix datalog/ HEAD
