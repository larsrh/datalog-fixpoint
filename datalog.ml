open Types
open Util
open List
open MyBat

type relation = string

type 'a symbol = {
	rel: relation;
	params: 'a exp list;
}

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
		let syms =
			if length clause.syms > 0
				then (map showNumSymbol clause.syms |> String.concat ", ") ^ ", "
				else "" in
		let constrs = map showConstr clause.constraints |> String.concat ", " in
		if String.length syms > 0 || String.length constrs > 0
			then head ^ " :- " ^ syms ^ constrs ^ "."
			else head ^ "."

end

module type Datalog = sig

	include DatalogTypes

	val fixpoint: clause list -> clause list

	val eval: number stringMap -> number constr -> bool

	val substitute: number exp stringMap -> number constr -> number constr result

	val contained: clause list -> relation -> number list -> bool

	val showClause: clause -> string

end


let test =
	let open OUnit in

	let testComposeExpMap _ =
		assert_equal ~cmp:(StringMap.equal (=))
			(fold_right2 StringMap.add ["y"; "x"] [Constant 1; Constant 0] StringMap.empty)
			(composeExpMap
				(fold_right2 StringMap.add ["y"; "x"] [Constant 1; Variable "u"] StringMap.empty)
				(fold_right2 StringMap.add ["u"; "v"] [Constant 0; Constant 0] StringMap.empty))
	in

	"Datalog" >::: [
		"composeExpMap" >:: testComposeExpMap
	]
