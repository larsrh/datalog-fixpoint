open Types

type relation = string

type 'a symbol = {
	rel: relation;
	params: 'a exp list;
}

module type Datalog = sig

	type number
	type 'a constr

	type clause = {
		head: number symbol;
		syms: number symbol list;
		constraints: number constr list;
	}

	val fixpoint: clause list -> clause list

	val eval: number stringMap -> number constr -> bool

	val substitute: number exp stringMap -> number constr -> number constr result

	val contained: clause list -> relation -> number list -> bool

end
