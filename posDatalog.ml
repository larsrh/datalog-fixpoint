open List

open MyBat
open Util
open Unification
open Datalog

type number = int

type op = GEQ | GR

type 'a posConstr = (var * 'a) list
type upperConstr = var

type 'a lhs = PosConstr of 'a posConstr | UpperConstr of upperConstr

(* GR will be transformed into GEQ *)
type 'a constr = {
	lhs: 'a lhs;
	rhs: 'a
}

type clause = {
	head: number symbol;
	syms: number symbol list;
	constraints: number constr list;
}

let simplify (constr: number posConstr) =
	let grouped = groupBy fst constr in
	let sumN (x, ns) = x, map snd ns |> fold_left (+) 0 in
	let pos (_, n) = n > 0 in
	map sumN grouped |> filter pos

let translateRHS b = function
| GR -> b + 1
| GEQ -> b

let mkPosConstraint cs op b =
	let non_neg (_, x) = x >= 0 in
	let simplified = map swap cs |> simplify in
	if for_all non_neg simplified
		then Some {lhs = PosConstr simplified; rhs = translateRHS b op}
		else None

let mkUpperBound v inclusive b =
	let op = if inclusive then GEQ else GR in
	{lhs = UpperConstr v; rhs = translateRHS (-b) op}

let evalConstr (constr: number constr) (assignment: var -> number) =
	let eval (var, num) = num * assignment var in
	let lhs =
		match constr.lhs with
		| PosConstr pos -> fold_left (+) 0 (map eval pos)
		| UpperConstr v -> -(assignment v) in
	lhs >= constr.rhs

(* Substitutes occurences of variables in a constraint by other variables or
   values and simplifies them accordingly. Returns `None' if the substitution
   produces an unsatisfiable formula. *)
(**let subst (mapping: (var * (var, number) either) list) (constr: number constr) =
	let testUpper var = assoc 
| Left 
| Right var -> assoc **)

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
	let containsX c = 
		match c.lhs with
		| PosConstr pos -> map fst pos |> mem x
		| UpperConstr v -> v = x in
	let pos, upper = partition isPos constrs in
	let posX, posNonX = partition containsX pos in
	let upperX, upperNonX = partition containsX upper in
	let elim ({lhs = PosConstr pos; rhs = a}, {rhs = b}) = removeX x b pos a in
	let newConstrs = cartesianProduct posX upperX |> map elim in
	posNonX @ upperNonX @ newConstrs

let fixpoint _ = raise (Failure "unimplemented")

let contained clauses relation nums =
	let strip (var, Constant num) = (var, num) in
	let assignment params = nums |> map (fun x -> Constant x) |> unify params |> mapOption (map strip |- flip assoc) in
	let isFact clause = length clause.syms = 0 && clause.head.rel = relation && length clause.head.params = length nums in
	let test constr = option (evalConstr constr) false in
	let testAll clause = for_all (assignment clause.head.params |> flip test) clause.constraints in
	exists testAll (filter isFact clauses)


(** Tests **)

let test =
	let open OUnit in

	let testSimplify _ =
		let expected = Some {lhs = PosConstr ["y", 1; "x", 3]; rhs = 3} in
		let actual = mkPosConstraint [1, "y"; 1, "x"; 2, "x"; -1, "z"; 1, "z"] GEQ 3 in
		assert_equal expected actual

	and testQElim _ =
		let expected = [{lhs = UpperConstr "y"; rhs = 2}; {lhs = PosConstr ["y", 1]; rhs = 18}] in
		let actual = qElim "x" [{lhs = UpperConstr "x"; rhs = 5}; {lhs = PosConstr ["y", 1; "x", 3]; rhs = 3}; {lhs = UpperConstr "y"; rhs = 2}] in
		assert_equal expected actual

	and testContains _ =
		let clauses = [{
			head = {
				rel = "R";
				params = [Variable "x"; Variable "y"; Constant 5]
			};
			syms = [];
			constraints = [
				mkUpperBound "x" false 3;
				mkPosConstraint [1, "y"] GR 2 |> getOption
			]
		}] in
		let shouldContain [x; y; z] = x < 3 && y > 2 && z = 5 in
		let check vals = assert_equal (shouldContain vals) (contained clauses "R" vals) in
		iter check (repeat [-4;-3;-2;-1;0;1;2;3;4;5;6] 3 |> nCartesianProduct)

	in

	"PosDatalog" >::: [
		"simplify" >:: testSimplify;
		"qElim" >:: testQElim;
		"contains" >:: testContains
	]
