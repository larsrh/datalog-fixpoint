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

	val contained: clause list -> relation -> number list -> bool

end
