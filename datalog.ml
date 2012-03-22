open Types
open Util
open List
open MyBat

type relation = string

type 'a symbol = {
	rel: relation;
	params: 'a exp list;
}

let symbolVars sym =
	let variable = function
	| Constant _ -> None
	| Variable v -> Some v in
	fold_right StringSet.add (collect variable sym.params) StringSet.empty

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

module Make(T: DatalogTypes) = struct

	open T

	let isFact clause = length clause.syms = 0
	let isRule clause = not (isFact clause)

	let quantifiedVars clause =
		let allConstrVars = fold_right StringSet.union (map constrVars clause.constraints) StringSet.empty in
		symbolVars clause.head |> StringSet.diff allConstrVars

end

module type Datalog = sig

	include DatalogTypes

	val fixpoint: clause list -> clause list

	val eval: number stringMap -> number constr -> bool

	val substitute: number exp stringMap -> number constr -> number constr result

	val contained: clause list -> relation -> number list -> bool

end
