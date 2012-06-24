Datalog fixpoints
=================

This software can be used to compute the fixpoint of [Datalog](http://en.wikipedia.org/wiki/Datalog) queries with linear constraints and uses the algorithm described by in ["Safe Datalog Queries with Linear Constraints"](http://cse.unl.edu/~revesz/publications/CP98.pdf) by Peter Z. Revesz. It only supports positive Datalog, but can be easily extended. The program is written in OCaml and uses ocamllex/ocamlyacc to parse the input.


Usage
-----

1. Build using `make`. Requires `ocamlfind`, `ocamlbuild` and [OUnit](http://ounit.forge.ocamlcore.org/).
2. To execute, run `make run` which is an alias for `./main.native`. The Datalog query is read from standard input and the result is printed to standard output. For syntax examples, look at the `tests/pos` directory.


Example
-------

Consider this simple query:

	t(z) :- z <= 1.
	s(x) :- t(z), z + x >= 2.

Running the program on this input will eliminate the quantified variable `z` in the body of `s(x)`. The result is:

	t(z) :- z <= 1.
	s(x) :- t(z), 1*z + 1*x >= 2.
	s(x) :- 1*x >= 1.

which is the fixpoint of the query. The result can be used as input for subsequent calls (e.g. after adding additional constraints). The output is unordered.


Documentation
-------------

To get an HTML version of the documentation, run `make doc` and point your browser to the file `src.docdir/index.html`.


Credits
-------

Some parts of the [OCaml "Batteries" library](http://batteries.forge.ocamlcore.org/) are used.
