open List

open MyBat
open Util
open Types
open Unification
open Datalog

type 'a posConstr = (var * 'a) list
type upperConstr = var

type 'a lhs = PosConstr of 'a posConstr | UpperConstr of upperConstr

module PosTypes = struct

	type number = int

	type 'a constr = {
		lhs: 'a lhs;
		rhs: 'a
	}

	type clause = {
		head: number symbol;
		syms: number symbol list;
		constraints: number constr list;
	}

	let constrVars constr = match constr.lhs with
	| PosConstr pos -> fold_right StringSet.add (map fst pos) StringSet.empty
	| UpperConstr v -> StringSet.singleton v

	let showNumber = string_of_int

	let showConstr constr = match constr.lhs with
	| PosConstr pos ->
		let f (v, n) = string_of_int n ^ "*" ^ v in
		let lhs = if length pos > 0
			then map f pos |> String.concat " + "
			else "0" in
		lhs ^ " >= " ^ string_of_int constr.rhs
	| UpperConstr v ->
		v ^ " <= " ^ string_of_int (-constr.rhs)

end

include PosTypes
module M = Make(PosTypes)
include M

let simplifyPosLHS (constr: number posConstr) =
	let grouped = groupBy fst constr in
	let sumN (x, ns) = x, map snd ns |> fold_left (+) 0 in
	let nonZero (x, n) = match n with
	| 0 -> None
	| _ when n > 0 -> Some (Some (x, n))
	| _ when n < 0 -> Some None
	| _ -> assert false (* avoid compiler warning *) in
	map sumN grouped |> collect nonZero |> sequenceList

let simplifyPos (constr: number posConstr) (rhs: number) =
	let f lhs = match lhs with
	| [] when 0 >= rhs -> Tautology
	| [] -> Contradiction
	| xs -> Result {lhs = PosConstr xs; rhs = rhs} in
	simplifyPosLHS constr |> mapOption f

let translateRHS b op =
	if op
		then b
		else b + 1

let sanitizeConstraints (constrs: number constr result list) =
	let f = function
	| Tautology -> None
	| Contradiction -> Some None
	| Result r -> Some (Some r) in
	collect f constrs |> sequenceList

let mkPosConstraint xs inclusive b =
	simplifyPos (map (fun (x, y) -> y, x) xs) (translateRHS b inclusive)

let mkUpperBound v inclusive b =
	{lhs = UpperConstr v; rhs = translateRHS (-b) inclusive}

let qElim (x: var) (constrs: number constr list) =
	let containsX c = constrVars c |> StringSet.mem x in
	let allX, allNonX = partition containsX constrs in

	let toPos c = match c.lhs with
	| PosConstr pos -> Some (pos, c.rhs)
	| UpperConstr _ -> None in
	let toUpper c = match c.lhs with
	| UpperConstr v -> Some c.rhs
	| PosConstr _ -> None in

	let pos = collect toPos allX in
	let upper = collect toUpper allX in

	let elim ((pos, a), b) =
		let rest = remove_assoc x pos in
		let c = assoc x pos in
		let rhs = a + c * b in
		simplifyPos rest rhs |> getOption in

	cartesianProduct pos upper |> map elim |>
	sanitizeConstraints |>
	mapOption (append allNonX)

let eval assignment constr =
	let eval (var, num) = num * (StringMap.find var assignment) in
	let lhs = match constr.lhs with
	| PosConstr pos -> fold_left (+) 0 (map eval pos)
	| UpperConstr v -> -(StringMap.find v assignment) in
	lhs >= constr.rhs

let substitute assignment constr =
	let replace var =
		if StringMap.mem var assignment
			then StringMap.find var assignment
			else Variable var in
	match constr.lhs with
	| PosConstr pos ->
		let f (acc, sum) (var, num) = match replace var with
		| Constant c -> acc, sum + c
		| Variable v -> (v, num) :: acc, sum in
		let acc, sum = fold_left f ([], 0) pos in
		let rhs = constr.rhs - sum in
		simplifyPos acc rhs |> getOption
	| UpperConstr upper -> match replace upper with
		| Constant c when -c >= constr.rhs -> Tautology
		| Constant c -> Contradiction
		| Variable v -> Result {constr with lhs = UpperConstr v}

(* produces a list of facts which can be derived from all facts in `clauses' and the specified `rule' *)
let applyRule (clauses: clause list) (rule: clause) =
	(* a fact is eligible for a symbol if the relation name and the arity match *)
	let findFacts sym = filterClauses sym.rel (length sym.params) clauses |> filter isFact in

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
				ownConstraints @ tupleConstraints |> sanitizeConstraints |> bindOption (fun constraints ->
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
					foldRightOption qElim elims (Some constraints) |> mapOption (fun eliminated ->
						let params = map (substituteExp canonical) rule.head.params in
						{head = {rule.head with params = params}; syms = []; constraints = eliminated}
					)
				)
			)
		) in

	collect solve product

let rec fixpoint clauses =
	let rules = filter isRule clauses in
	let newFacts = map (applyRule clauses) rules |> concat in
	let f acc fact = if mem fact acc then acc else fact :: acc in
	let inserted = fold_left f clauses newFacts in
	if length inserted > length clauses
		then fixpoint inserted
		else clauses

let contained clauses relation nums =
	let assignment params =
		let f assgn (exp, num) = match exp with
		| Constant c ->
			if c = num
				then Some assgn
				else None
		| Variable v ->
			if StringMap.mem v assgn
				then if StringMap.find v assgn = num
					then Some assgn
					else None
				else Some (StringMap.add v num assgn) in
		combine params nums |> foldLeftOption f (Some StringMap.empty) in
	let test constr = function
	| Some assgn -> eval assgn constr
	| None -> false in
	let testAll clause = for_all (assignment clause.head.params |> flip test) clause.constraints in
	filterClauses relation (length nums) clauses |> filter isFact |> exists testAll


(** Tests **)

let test =
	let open OUnit in

	let _mkPosConstraint xs inclusive b = match mkPosConstraint xs inclusive b with
	| Some (Result r) -> r
	| _ -> assert false in

	let testSimplify _ =
		let expected = Some (Result {lhs = PosConstr ["y", 1; "x", 3]; rhs = 3}) in
		let actual = mkPosConstraint [1, "y"; 1, "x"; 2, "x"; -1, "z"; 1, "z"] true 3 in
		assert_equal expected actual
	in

	let testQElim _ =
		let expected = Some [{lhs = UpperConstr "y"; rhs = 2}; {lhs = PosConstr ["y", 1]; rhs = 18}] in
		let actual = qElim "x" [{lhs = UpperConstr "x"; rhs = 5}; {lhs = PosConstr ["y", 1; "x", 3]; rhs = 3}; {lhs = UpperConstr "y"; rhs = 2}] in
		assert_equal expected actual
	in

	let testContains _ =
		let clauses = [{
			head = {rel = "R"; params = [Variable "x"; Variable "y"; Constant 5]};
			syms = [];
			constraints = [mkUpperBound "x" false 3; _mkPosConstraint [1, "y"] false 2]
		}] in
		let shouldContain [x; y; z] = x < 3 && y > 2 && z = 5 in
		let check vals = assert_equal (shouldContain vals) (contained clauses "R" vals) in
		let rec repeat x = function
		| 0 -> []
		| n -> x :: repeat x (n-1) in
		iter check (repeat [-4;-3;-2;-1;0;1;2;3;4;5;6] 3 |> nCartesianProduct)
	in

	let testApplyRule _ =
		let facts = [{
			head = {rel = "R"; params = [Variable "x"; Variable "y"; Constant 5]};
			syms = [];
			constraints = [mkUpperBound "x" false 6; _mkPosConstraint [1, "y"] false 2]
		}; {
			head = {rel = "T"; params = [Variable "x"; Variable "x"]};
			syms = [];
			constraints = []
		}] in
		let rule = {
			head = {rel = "S"; params = [Variable "z"]};
			syms = [{rel = "R"; params = [Variable "s"; Variable "s"; Variable "z"]}; {rel = "T"; params = [Variable "s"; Variable "z"]}];
			constraints = [mkUpperBound "z" false 10]
		} in
		let clauses = rule :: facts in
		assert_equal [{
			head = {rel = "S"; params = [Constant 5]};
			syms = [];
			constraints = []
		}] (applyRule clauses rule)
	in

	let testFixpoint _ =
		let clauses = [{
			head = {rel = "R"; params = [Variable "x"; Variable "y"; Constant 5]};
			syms = [];
			constraints = [mkUpperBound "x" false 6; _mkPosConstraint [1, "y"] false 2]
		}; {
			head = {rel = "T"; params = [Variable "x"; Variable "x"]};
			syms = [];
			constraints = []
		}; {
			head = {rel = "S"; params = [Variable "z"]};
			syms = [{rel = "R"; params = [Variable "s"; Variable "s"; Variable "z"]}; {rel = "T"; params = [Variable "s"; Variable "z"]}];
			constraints = [mkUpperBound "z" false 10]
		}; {
			head = {rel = "U"; params = [Variable "r"]};
			syms = [{rel = "S"; params = [Variable "r"]}];
			constraints = [mkUpperBound "r" false 9]
		}] in
		let newFacts = [{
			head = {rel = "U"; params = [Constant 5]};
			syms = [];
			constraints = []
		}; {
			head = {rel = "S"; params = [Constant 5]};
			syms = [];
			constraints = []
		}] in
		assert_equal (newFacts @ clauses) (fixpoint clauses)
	in

	"PosDatalog" >::: [
		"simplify" >:: testSimplify;
		"qElim" >:: testQElim;
		"contains" >:: testContains;
		"applyRule" >:: testApplyRule;
		"fixpoint" >:: testFixpoint
	]
