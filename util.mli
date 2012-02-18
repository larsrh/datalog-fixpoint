open Datalog

val some: 'a -> 'a option

val lookup: 'a -> ('a * 'b) list -> 'b option

val swap: 'a * 'b -> 'b * 'a

val repeat: 'a -> int -> 'a list

(*
	Matches a list of atoms against another list of atoms. An atom can be a
	variable or a value. This corresponds to a restricted form of unification
	of the head of a rule (first parameter) and the parameters of a relation
	occuring on the right-hand side of a rule.

	Example:

		R(x, y, z, 0).

		X(u, v) :- R(u, v, 1, 0).

	An application of `unify' generates the mapping `[x -> u; y -> v; z -> 1]'.

	This function is not symmetric, as it does not generate "true" equality
	constraints. For example, the head `[x; x]' cannot be instantiated with
	`[x; y]' because this would require that `x' is equal to `y'. However, the
	head `[x; y]' can be matched with `[x; x]'.

	For both parameters, `Left` corresponds to a constant and `Right` to a
	variable.
*)
val unify: 'a exp list -> 'a exp list -> (var * 'a exp) list option


val test: OUnit.test
