(** Definition of basic types. *)

(** The type of variables. Note that there is no distinction between lower or
    upper case identifiers. *)
type var = string

(** An expression parametrized on the type of constants. *)
type 'a exp = Constant of 'a | Variable of var

(** Result of a computation which either terminated normally or produced a
    tautology (always true) or a contradiction (always false). *)
type 'a result = Result of 'a | Tautology | Contradiction

(** The module for sets of [string]s. *)
module StringSet = Set.Make(String)

(** The module for maps with [string] keys. *)
module StringMap = Map.Make(String)

(** The type of a set of [string]s. *)
type stringSet = StringSet.t

(** The type of a map with [string] keys parametrized on the type of the
    values. *)
type 'a stringMap = 'a StringMap.t

(** A {!Unification} equality constraint. Denotes that a set of variables must
    be equal and optionally equal to a constant. Equalities between a set of
	variables and multiple constants are not even generated, because they can
	trivially be reduced to a contradiction or to an equality with only one
	constant. *)
type 'a equality = {
	number: 'a option;
	vars: stringSet
}

(** The result of a {{!Unification.unify}unification}. See
    {!Unification.unify} for details. *)
type 'a mapping = {
	assignment: 'a exp stringMap;
	equalities: 'a equality list
}
