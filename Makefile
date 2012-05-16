OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build

all: clean test build doc package-src

build:
	$(OCAMLBUILD) main.native

run:
	$(OCAMLBUILD) main.native --

test:
	$(OCAMLBUILD) test.native --

debug:
	$(OCAMLBUILD) -tag debug main.byte
	rlwrap ocamldebug -I _build main.byte

debug-test:
	$(OCAMLBUILD) -tag debug test.byte
	rlwrap ocamldebug `ocamlfind query -recursive -i-format oUnit` -I _build test.byte

doc:
	$(OCAMLBUILD) src.docdir/index.html

clean:
	$(OCAMLBUILD) -clean

package-src:
	git archive --format zip --output impl.zip --prefix datalog/ HEAD
