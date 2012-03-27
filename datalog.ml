open Types
open Util
open Unification
open List
open MyBat

let showExp f = function
| Constant c -> f c
| Variable v -> v

let showSymbol f sym =
	if length sym.params > 0
		then
			let params = map (showExp f) sym.params |> String.concat "," in
			sym.rel ^ "(" ^ params ^ ")"
		else
			sym.rel

let symbolVars sym =
	let variable = function
	| Constant _ -> None
	| Variable v -> Some v in
	fold_right StringSet.add (collect variable sym.params) StringSet.empty

let substituteExp map = function
| Constant c -> Constant c
| Variable v ->
	if StringMap.mem v map
		then StringMap.find v map
		else Variable v

let composeExpMap m1 m2 = StringMap.map (substituteExp m2) m1

module type DatalogTypes = sig

	type number
	type constr

	type clause = {
		head: number symbol;
		syms: number symbol list;
		constraints: constr list;
	}

	val constrVars: constr -> stringSet

	val showNumber: number -> string

	val showConstr: constr -> string

	val elimVar: var -> constr list -> constr list option

	val eval: number stringMap -> constr -> bool

	val substitute: number exp stringMap -> constr -> constr result

end

module Make(T: DatalogTypes) = struct

	open T

	let allConstrVars clause =
		map constrVars clause.constraints |> fold_left StringSet.union StringSet.empty

	let isFact clause =
		length clause.syms = 0 &&
		StringSet.subset (allConstrVars clause) (symbolVars clause.head)

	let isRule clause = not (isFact clause)

	let quantifiedVars clause =
		StringSet.diff (allConstrVars clause) (symbolVars clause.head)

	let filterClauses rel arity =
		let eligible clause = clause.head.rel = rel && length clause.head.params = arity in
		filter eligible

	let showNumExp = showExp showNumber
	let showNumSymbol = showSymbol showNumber

	let showClause clause =
		let head = showNumSymbol clause.head in
		let syms = map showNumSymbol clause.syms in
		let constrs = map showConstr clause.constraints in
		let body = syms @ constrs |> String.concat ", " in
		if String.length body > 0
			then head ^ " :- " ^ body ^ "."
			else head ^ "."

	(* produces a list of facts which can be derived from all `facts' and the specified `rule' *)
	let applyRule (facts: clause list) (rule: clause) =
		(* a fact is eligible for a symbol if the relation name and the arity match *)
		let findFacts sym = filterClauses sym.rel (length sym.params) facts in

		(* a list of all possible matching facts for each symbol in the rule *)
		let facts = map findFacts rule.syms in

		(* generate all possible tuples *)
		let product = nCartesianProduct facts in

		(* solve one possible tuple: unify all symbols, merge equalities, substitute, eliminate quantifiers *)
		let solve tuple =
			(* list of list of parameters *)
			let tupleParamss = map (fun c -> c.head.params) tuple in
			let ownParamss = map (fun s -> s.params) rule.syms in

			(* unify corresponding pairs of parameters (`x': head of the fact, `y': symbol in the body of the rule) *)
			(* sequence the result: continue only iff all unifications succeeded *)
			combine tupleParamss ownParamss |> map (fun (x, y) -> unify x y) |> sequenceList |> bindOption (fun mappings ->
				(* from the unification results, obtain all equalitiy sets and merge those *)
				(* continue only iff this doesn't produce a contradiction *)
				map (fun m -> m.equalities) mappings |> mergeEqualities |> bindOption (fun equalities ->
					(* a fresh identifier for each equality which does not contain a number *)
					let canonical = canonicalizeVars equalities in

					(* for each fact, compute the final assignment by composing the original assignment with the canonical substitution *)
					let assignments = map (fun m -> composeExpMap m.assignment canonical) mappings in

					(* substitute all constraints in the facts *)
					let substituteAll (assignment, clause) = map (substitute assignment) clause.constraints in
					let tupleConstraints = combine assignments tuple |> map substituteAll |> concat in

					(* substitute all constraints in this rule *)
					let ownConstraints = map (substitute canonical) rule.constraints in

					(* sanitize the new constraints *)
					(* only continue iff no contradictions have been generated *)
					ownConstraints @ tupleConstraints |> getResults |> bindOption (fun constraints ->
						(* set of quantified variables *)
						let quantified = quantifiedVars rule in

						(* lookup the quantified variables in the canonical substitution *)
						let addToSet var vars =
							if StringMap.mem var canonical
								then match StringMap.find var canonical with
								| Constant _ -> vars
								| Variable v -> StringSet.add v vars
							else
								StringSet.add var vars in
						let elims = StringSet.fold addToSet quantified StringSet.empty |> StringSet.elements in

						(* eliminate the distinct canonicalized variables and produce a new fact accordingly *)
						foldRightOption elimVar elims (Some constraints) |> mapOption (fun eliminated ->
							let params = map (substituteExp canonical) rule.head.params in
							{head = {rule.head with params = params}; syms = []; constraints = eliminated}
						)
					)
				)
			) in

		collect solve product

	let fixpoint clauses =
		let rules = filter isRule clauses in
		let rec aux facts =
			let newFacts = map (applyRule facts) rules |> concat in
			let f acc fact = if mem fact acc then acc else fact :: acc in
			let inserted = fold_left f facts newFacts in
			if length inserted > length facts
				then aux inserted
				else facts in
		rules @ aux (filter isFact clauses)

end

module type Interface = sig

	include DatalogTypes

	val fixpoint: clause list -> clause list

	val contained: clause list -> relation -> number list -> bool

	val showClause: clause -> string

end
