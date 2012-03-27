(** Common structures for {!Datalog} instances. *)

open Types

module Int: sig
	type number = int
	val showNumber: number -> string
end

module LinearConstraint: sig
	type 'a linearConstraint = (var * 'a) list
	val showLinearConstr: ('a -> string) -> 'a -> 'a linearConstraint -> string
	val linearConstrVars: 'a linearConstraint -> stringSet
end
