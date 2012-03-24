all: build

OCAMLBUILD=ocamlbuild -use-ocamlfind
DEBUG=main.byte
FILE=main.native

build:
	$(OCAMLBUILD) $(FILE)

run:
	$(OCAMLBUILD) $(FILE) --

test:
	$(OCAMLBUILD) test.native --

debug:
	$(OCAMLBUILD) -tag debug $(DEBUG)
	rlwrap ocamldebug -I _build $(DEBUG)

clean:
	$(OCAMLBUILD) -clean

package:
	git archive --format zip --output impl.zip --prefix datalog/ HEAD
