type var = string

type relation = string

type 'a exp = Constant of 'a | Variable of var

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

end;;
