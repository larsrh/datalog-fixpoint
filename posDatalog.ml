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
open M

let simplifyPos (constr: number posConstr) =
	let grouped = groupBy fst constr in
	let sumN (x, ns) = x, map snd ns |> fold_left (+) 0 in
	let pos (_, n) = n > 0 in
	map sumN grouped |> filter pos

let translateRHS b op =
	if op
		then b
		else b + 1

let mkPosConstraint cs inclusive b =
	let non_neg (_, x) = x >= 0 in
	let simplified = map swap cs |> simplifyPos in
	if for_all non_neg simplified
		then Some {lhs = PosConstr simplified; rhs = translateRHS b inclusive}
		else None

let mkUpperBound v inclusive b =
	{lhs = UpperConstr v; rhs = translateRHS (-b) inclusive}

(* Transforms -x >= b and c * x + d_i * y_i >= a into d_i * y_i >= a + c * b *)
let removeX (x: var) (b: number) (constr: number posConstr) (a: number) =
	let rest = remove_assoc x constr in
	let c = assoc x constr in
	{lhs = PosConstr rest; rhs = a + c * b}

let qElim (x: var) (constrs: number constr list) =
	let isPos c =
		match c.lhs with
		| PosConstr _ -> true
		| UpperConstr _ -> false in
	let containsX c = constrVars c |> StringSet.mem x in
	let pos, upper = partition isPos constrs in
	let posX, posNonX = partition containsX pos in
	let upperX, upperNonX = partition containsX upper in
	let elim ({lhs = PosConstr pos; rhs = a}, {rhs = b}) = removeX x b pos a in (* TODO use proper simplification *)
	let newConstrs = cartesianProduct posX upperX |> map elim in
	posNonX @ upperNonX @ newConstrs

let eval assignment constr =
	let eval (var, num) = num * (StringMap.find var assignment) in
	let lhs =
		match constr.lhs with
		| PosConstr pos -> fold_left (+) 0 (map eval pos)
		| UpperConstr v -> -(StringMap.find v assignment) in
	lhs >= constr.rhs

let substitute assignment constr = match constr.lhs with
| PosConstr pos ->
	let f (acc, sum) (var, num) = match StringMap.find var assignment with
	| Constant c -> acc, sum + c
	| Variable v -> (v, num) :: acc, sum in
	let acc, sum = fold_left f ([], 0) pos in
	let rhs' = constr.rhs - sum in
	(match simplifyPos acc with
	| [] when rhs' <= 0 -> Tautology
	| [] -> Contradiction
	| xs -> Result {lhs = PosConstr xs; rhs = rhs'})
| UpperConstr upper -> match StringMap.find upper assignment with
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

	let solve tuple =
		let tupleParams = map (fun c -> c.head.params) tuple in
		let ownParams = map (fun s -> s.params) rule.syms in
		combine tupleParams ownParams |> map (fun (x, y) -> unify x y) |> sequenceList |> bindOption (fun mappings ->
			map (fun m -> m.equalities) mappings |> mergeEqualities |> bindOption (fun equalities ->
				let canonical = canonicalizeVars equalities in
				let assignments = map (fun m -> composeExpMap m.assignment canonical) mappings in

				(* get rid of tautologies *)
				let filterConstraint = function
				| Tautology -> None
				| Contradiction -> Some None
				| Result r -> Some (Some r) in

				let substituteAll (assignment, clause) = map (substitute assignment) clause.constraints in
				let tupleConstraints = combine assignments tuple |> map substituteAll |> concat in
				let ownConstraints = map (substitute canonical) rule.constraints in
				ownConstraints @ tupleConstraints |> collect filterConstraint |> sequenceList |> mapOption (fun constraints ->
					let elim var constrs =
						if StringMap.mem var canonical
							then match (StringMap.find var canonical) with
							| Constant _ -> constrs
							| Variable v -> qElim v constrs
							else constrs in
					let eliminated = StringSet.fold elim (quantifiedVars rule) constraints in
					let params = map (substituteExp canonical) rule.head.params in
					{head = {rule.head with params = params}; syms = []; constraints = eliminated}
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

	let testSimplify _ =
		let expected = Some {lhs = PosConstr ["y", 1; "x", 3]; rhs = 3} in
		let actual = mkPosConstraint [1, "y"; 1, "x"; 2, "x"; -1, "z"; 1, "z"] true 3 in
		assert_equal expected actual
	in

	let testQElim _ =
		let expected = [{lhs = UpperConstr "y"; rhs = 2}; {lhs = PosConstr ["y", 1]; rhs = 18}] in
		let actual = qElim "x" [{lhs = UpperConstr "x"; rhs = 5}; {lhs = PosConstr ["y", 1; "x", 3]; rhs = 3}; {lhs = UpperConstr "y"; rhs = 2}] in
		assert_equal expected actual
	in

	let testContains _ =
		let clauses = [{
			head = {rel = "R"; params = [Variable "x"; Variable "y"; Constant 5]};
			syms = [];
			constraints = [mkUpperBound "x" false 3; mkPosConstraint [1, "y"] false 2 |> getOption]
		}] in
		let shouldContain [x; y; z] = x < 3 && y > 2 && z = 5 in
		let check vals = assert_equal (shouldContain vals) (contained clauses "R" vals) in
		iter check (repeat [-4;-3;-2;-1;0;1;2;3;4;5;6] 3 |> nCartesianProduct)
	in

	let testApplyRule _ =
		let facts = [{
			head = {rel = "R"; params = [Variable "x"; Variable "y"; Constant 5]};
			syms = [];
			constraints = [mkUpperBound "x" false 6; mkPosConstraint [1, "y"] false 2 |> getOption]
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
			constraints = [mkUpperBound "x" false 6; mkPosConstraint [1, "y"] false 2 |> getOption]
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
