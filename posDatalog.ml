open Batteries_uni
open List

open Util
open Datalog

type number = int

type op = GEQ | GR

type 'a posConstr = ('a * var) list
type upperConstr = var

(* GR will be transformed into GEQ *)
type 'a constr = {
	lhs: ('a posConstr, upperConstr) either;
	rhs: number
}

type clause = {
	head: number symbol;
	syms: number symbol list;
	constraints: number constr list;
}

let translateRHS b = function
| GR -> b + 1
| GEQ -> b

let mkNumber x = x

let mkPosConstraint cs op b =
	let non_neg (x, _) = x >= 0 in
	if for_all non_neg cs
		then Some {lhs = Left cs; rhs = translateRHS b op}
		else None

let mkUpperBound v inclusive b =
	let op = if inclusive then GEQ else GR in
	{lhs = Right v; rhs = translateRHS (-b) op}

let evalConstr (constr: number constr) (assignment: var -> number) =
	let eval (num, var) = num * assignment var in
	let pos cs = fold_left (+) 0 (map eval cs)
	and upper var = -(assignment var) in
	either pos upper constr.lhs >= constr.rhs

let fixpoint _ = raise (Failure "unimplemented")

let contained clauses relation nums =
	let strip (var, Left num) = (var, num) in
	let assignment params = map left nums |> unify params |> Option.map (map strip |- flip assoc)
	and isFact clause = length clause.syms = 0 && clause.head.rel = relation && length clause.head.params = length nums
	and test constr = Option.map_default (evalConstr constr) false in
	let testAll clause = for_all (assignment clause.head.params |> flip test) clause.constraints in
	exists testAll (filter isFact clauses)
