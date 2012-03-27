(** A generic interface to Datalog databases and corresponding computations. *)

open Types

(** Converts an {{!Types.exp}expression} to a [string] using the specified
    function to convert constants. *)
val showExp: ('a -> string) -> 'a exp -> string

(** Converts a {!symbol} to a [string] using the specified function to convert
    constants. *)
val showSymbol: ('a -> string) -> 'a symbol -> string

(** Computes the set of variables occuring in the parameter list of a
    {!symbol}. *)
val symbolVars: 'a symbol -> stringSet

(** Replaces an occurence of a variable in an {{!Types.exp}expression}. *)
val substituteExp: 'a exp stringMap -> 'a exp -> 'a exp

(** The result of [composeExpMap m1 m2] is a new map, where the values of [m1]
    are substituted using [m2]. *)
val composeExpMap: 'a exp stringMap -> 'a exp stringMap -> 'a exp stringMap

(** Basic types and functions for a Datalog instance. *)
module type DatalogTypes = sig

	(** The type of constants. *)
	type number

	(** The type of constraints. Instances are expected to provide a function
	    which produces values of this abstract type. *)
	type constr

	(** The type of clauses, consisting of a head and a body, which consists of
	    symbols and constraints. *)
	type clause = {
		head: number symbol;
		syms: number symbol list;
		constraints: constr list;
	}

	(** The set of variables in a constraint. *)
	val constrVars: constr -> stringSet

	(** Converts a constant to a string. *)
	val showNumber: number -> string

	(** Converts a constraint to a string. *)
	val showConstr: constr -> string

	(** Eliminate an existentially quantified variable from a list of clauses.
	    It is guaranteed that if [Some y = elimVar x cs] holds, [x] is not an
	    element of [elimVar x cs]. A result of [None] denotes a
	    contradiction. *)
	val elimVar: var -> constr list -> constr list option

	(** Evaluates a constraint by substituting all occuring variables in the
	    constraint and returning [true] iff the assignment satisfies the
		constraint. Raises [Not_found] when the assignment does not contain all
	    occuring variables. *)
	val eval: number stringMap -> constr -> bool

	(** Substitutes (not necessarily all) variables in a constraint. *)
	val substitute: number exp stringMap -> constr -> constr result

end

(** Provides utility functions implementations based on an instance of
    {!DatalogTypes}. *)
module Make(T: DatalogTypes): sig

	(** Returns [true] iff the clause is a fact. See {!isRule}. *)
	val isFact: T.clause -> bool

	(** Returns [true] iff the clause is a rule. A clause is either a fact or
	    a rule. A clause is a rule iff it contains symbols in the body or there
	    is a variable occuring in the constraints but not in the head. *)
	val isRule: T.clause -> bool

	(** The set of quantified variables. A variable is quantified if it occurs
	    in the body of a rule but not in the head. *)
	val quantifiedVars: T.clause -> stringSet

	(** Filters a list of clauses by its relation (i.e. the relation identifier
	    of the head) and the arity (i.e. the number of the parameters of the
	    head). *)
	val filterClauses: relation -> int -> T.clause list -> T.clause list

	(** Converts an expression to a [string]. Equivalent to
	    [showExp T.showNumber]. *)
	val showNumExp: T.number exp -> string

	(** Converts a symbol to a [string]. Equivalent to
	    [showSymbol T.showNumber]. *)
	val showNumSymbol: T.number symbol -> string

	(** Converts a clause to a [string]. *)
	val showClause: T.clause -> string

	(** Compute the fixpoint of a database. This function is idempotent. The
	    behaviour on lists containing duplicate elements is unspecified. *)
	val fixpoint: T.clause list -> T.clause list

end

(** High-level interface to Datalog database operations. *)
module type Interface = sig

	include DatalogTypes

	(** Compute the fixpoint of a database. This function is idempotent. The
	    behaviour on lists containing duplicate elements is unspecified. *)
	val fixpoint: clause list -> clause list

	(** [contained cs r xs] checks whether the tuple [xs] is element of the
	    relation [r] in the database [cs]. *)
	val contained: clause list -> relation -> number list -> bool

	(** Converts a clause to a [string]. *)
	val showClause: clause -> string

end
