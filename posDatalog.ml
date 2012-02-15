open Batteries_uni
open List

open Util
open Datalog

type number = int

type op = GEQ | GR

type 'a posConstr = (var * 'a) list
type upperConstr = var

(* GR will be transformed into GEQ *)
type 'a constr = {
	lhs: ('a posConstr, upperConstr) either;
	rhs: 'a
}

type clause = {
	head: number symbol;
	syms: number symbol list;
	constraints: number constr list;
}

let simplify (constr: number posConstr) =
	let cmp (x, _) (y, _) = compare x y in
	let grouped = group cmp constr
	and pair xs = hd xs |> fst, map snd xs |> sum
	and pos (_, n) = n > 0 in
	map pair grouped |> filter pos

let translateRHS b = function
| GR -> b + 1
| GEQ -> b

let mkPosConstraint cs op b =
	let non_neg (x, _) = x >= 0 in
	if for_all non_neg cs
		then Some {lhs = map swap cs |> simplify |> left; rhs = translateRHS b op}
		else None

let mkUpperBound v inclusive b =
	let op = if inclusive then GEQ else GR in
	{lhs = Right v; rhs = translateRHS (-b) op}

let evalConstr (constr: number constr) (assignment: var -> number) =
	let eval (var, num) = num * assignment var in
	let pos cs = fold_left (+) 0 (map eval cs)
	and upper var = -(assignment var) in
	either pos upper constr.lhs >= constr.rhs

(* Transforms -x >= b and c * x + d_i * y_i >= a into d_i * y_i >= a + c * b *)
let removeX (x: var) (b: number) (constr: number posConstr) (a: number) =
	let rest = remove_assoc x constr
	and c = assoc x constr in
	{lhs = Left rest; rhs = a + c * b}

let qElim (x: var) (constraints: number constr list) =
	let isPos c = either (const true) (const false) c.lhs
	and containsX c = either (map fst |- mem x) ((=) x) c.lhs in
	let pos, upper = partition isPos constraints in
	let posX, posNonX = partition containsX pos
	and upperX, upperNonX = partition containsX upper
	and elim ({lhs = Left pos; rhs = a}, {rhs = b}) = removeX x b pos a in
	let newConstrs = cartesian_product posX upperX |> map elim in
	posNonX @ upperNonX @ newConstrs

let fixpoint _ = raise (Failure "unimplemented")

let contained clauses relation nums =
	let strip (var, Left num) = (var, num) in
	let assignment params = map left nums |> unify params |> Option.map (map strip |- flip assoc)
	and isFact clause = length clause.syms = 0 && clause.head.rel = relation && length clause.head.params = length nums
	and test constr = Option.map_default (evalConstr constr) false in
	let testAll clause = for_all (assignment clause.head.params |> flip test) clause.constraints in
	exists testAll (filter isFact clauses)


(** Tests **)

let test =
	let open OUnit in

	let testQElim _ =
		let expected = [{lhs = Right "y"; rhs = 2}; {lhs = Left ["y", 1]; rhs = 18}]
		and actual = qElim "x" [{lhs = Right "x"; rhs = 5}; {lhs = Left ["y", 1; "x", 3]; rhs = 3}; {lhs = Right "y"; rhs = 2}] in
		assert_equal expected actual

	and testContains _ =
		let clauses = [{
			head = {
				rel = "R";
				params = [Right "x"; Right "y"; Left 5]
			};
			syms = [];
			constraints = [
				mkUpperBound "x" false 3;
				Option.get (mkPosConstraint [1, "y"] GR 2)
			]
		}]
		and shouldContain [x; y; z] = x < 3 && y > 2 && z = 5 in
		let check vals = assert_equal (shouldContain vals) (contained clauses "R" vals) in
		iter check (repeat (of_enum (-4--6)) 3 |> n_cartesian_product)

	in

	"PosDatalog" >::: [
		"qElim" >:: testQElim;
		"contains" >:: testContains
	]
