open Types

type relation = string

type 'a symbol = {
	rel: relation;
	params: 'a exp list;
}

val showExp: ('a -> string) -> 'a exp -> string

val showSymbol: ('a -> string) -> 'a symbol -> string

val symbolVars: 'a symbol -> stringSet

val substituteExp: 'a exp stringMap -> 'a exp -> 'a exp

val composeExpMap: 'a exp stringMap -> 'a exp stringMap -> 'a exp stringMap

module type DatalogTypes = sig

	type number
	type 'a constr

	type clause = {
		head: number symbol;
		syms: number symbol list;
		constraints: number constr list;
	}

	val constrVars: number constr -> stringSet

	val showNumber: number -> string

	val showConstr: number constr -> string

end

module Make(T: DatalogTypes): sig

	val isFact: T.clause -> bool
	val isRule: T.clause -> bool

	val quantifiedVars: T.clause -> stringSet

	val filterClauses: relation -> int -> T.clause list -> T.clause list

	val showNumExp: T.number exp -> string

	val showNumSymbol: T.number symbol -> string

	val showClause: T.clause -> string

end

module type Datalog = sig

	include DatalogTypes

	val fixpoint: clause list -> clause list

	val eval: number stringMap -> number constr -> bool

	val substitute: number exp stringMap -> number constr -> number constr result

	val contained: clause list -> relation -> number list -> bool

	val showClause: clause -> string

end


val test: OUnit.test
