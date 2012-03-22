open Types

type relation = string

type 'a symbol = {
	rel: relation;
	params: 'a exp list;
}

val symbolVars: 'a symbol -> stringSet

module type DatalogTypes = sig

	type number
	type 'a constr

	type clause = {
		head: number symbol;
		syms: number symbol list;
		constraints: number constr list;
	}

	val constrVars: number constr -> stringSet

end

module Make(T: DatalogTypes): sig

	val isFact: T.clause -> bool
	val isRule: T.clause -> bool

	val quantifiedVars: T.clause -> stringSet

end

module type Datalog = sig

	include DatalogTypes

	val fixpoint: clause list -> clause list

	val eval: number stringMap -> number constr -> bool

	val substitute: number exp stringMap -> number constr -> number constr result

	val contained: clause list -> relation -> number list -> bool

end
