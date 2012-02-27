open List

open Datalog
open MyBat
open Util

let freshVar set = StringSet.max_elt set ^ "_"

type 'a equality = {
	number: 'a option;
	vars: stringSet
}

type 'a mapping = {
	assignment: 'a exp stringMap;
	equalities: 'a equality list
}

let unify l1 l2 =
	let combined = combine l1 l2 in
	let f constrs = function
	| Constant c1, Constant c2 ->
		if c1 = c2
			then Some constrs
			else None
	| Constant c, Variable _ ->
		None
	| Variable v, param ->
		match lookup v constrs with
		| None -> Some ((v, param) :: constrs)
		| Some binding when param = binding -> Some constrs
		| Some _ -> None in
	foldLeftOption f (Some []) combined

(* Adds an equality to a given list of equalities. This operation is commutative. *)
let addEquality (eqs: 'a equality list) (e1: 'a exp) (e2: 'a exp) =

	(* Finds the equality which contains the specified variable. By convention,
	   there should be at most one such equality at any point in time. Returns
	   the found equality and the remaining list of equalities. *)
	let find var =
		match partition (fun eq -> StringSet.mem var eq.vars) eqs with
		| [], _ -> None
		| [e], xs -> Some (e, xs)
		| _ -> assert false in
	
	(* Simply add a new equality to the existing set. *)
	let add number vars eqs = Some ({number = number; vars = vars} :: eqs) in

	(* The same equalities as before. *)
	let unchanged = Some eqs in

	(* An equality constraint over two constants can only be satisfied if both
	   are equal. In that case, no new constraint is generated. Otherwise,
	   the whole set of equalities can't be satisfied any more. *)
	let constant_constant c1 c2 =
		if c1 = c2
			then unchanged
			else None
	in

	(* An equality constraint over a variable and a constant is satisfiable if
	   the variable is not yet equal to a constant or it is equal to the constant
	   already. *)
	let constant_variable c v =
		match find v with
		| None -> add (Some c) (StringSet.singleton v) eqs
		| Some ({number = Some n; vars = vars}, _) -> 
			if n = c
				then unchanged
				else None
		| Some ({vars = vars}, xs) -> add (Some c) vars xs
	in

	(* If there are two distinct equalities, we have to merge them, depending 
	   on whether they contain constants. If both do and they mismatch,
	   it is unsatisfiable. Otherwise, it's just the constant and the union of
	   the variable sets. *)
	let variable_variable v1 v2 =
		match find v1, find v2 with
		| None, None ->
			add None (fold_right StringSet.add [v1; v2] StringSet.empty) eqs

		| None, Some (eq, xs) ->
			Some ({eq with vars = StringSet.add v1 eq.vars} :: xs)

		| Some (eq, xs), None ->
			Some ({eq with vars = StringSet.add v2 eq.vars} :: xs)

		| Some (eq1, _), Some (eq2, _) ->
			if eq1 = eq2
				then unchanged
				else
					let rest = filter (fun eq -> eq != eq1 && eq != eq2) eqs in
					let return num = add num (StringSet.union eq1.vars eq2.vars) rest in
					match eq1.number, eq2.number with
					| None, None -> return None
					| None, Some n -> return (Some n)
					| Some n, None -> return (Some n)
					| Some n, Some m ->
						if n = m
							then return (Some n)
							else None
	in

	match e1, e2 with
	| Constant c1, Constant c2 -> constant_constant c1 c2
	| Constant c, Variable v -> constant_variable c v
	| Variable v, Constant c -> constant_variable c v
	| Variable v1, Variable v2 -> variable_variable v1 v2
	

let test = 
	let open OUnit in

	let testAddEquality _ =
		assert_equal (Some [{number = Some 3; vars = StringSet.singleton "x"}]) (addEquality [{number = None; vars = StringSet.singleton "x"}] (Constant 3) (Variable "x")); 
		assert_equal (Some [{number = Some 3; vars = StringSet.singleton "x"}]) (addEquality [] (Constant 3) (Variable "x"));
		assert_equal (Some [{number = None; vars = fold_right StringSet.add ["y"; "x"] StringSet.empty}]) (addEquality [{number = None; vars = StringSet.singleton "x"}] (Variable "y") (Variable "x"));
		assert_equal (Some [{number = None; vars = fold_right StringSet.add ["x"; "y"] StringSet.empty}]) (addEquality [{number = None; vars = StringSet.singleton "x"}; {number = None; vars = StringSet.singleton "y"}] (Variable "y") (Variable "x"))

	in

	"Unification" >::: [
		"addEquality" >:: testAddEquality
	]
(*
	let testUnify _ =
		assert_equal None (unify [Variable "a"; Variable "a"] [Variable "a"; Variable "b"]);
		assert_equal (Some ["b", Variable "a"]) (unify [Variable "b"; Variable "b"] [Variable "a"; Variable "a"]);
		assert_equal (Some ["a", Constant 3]) (unify [Variable "a"; Variable "a"] [Constant 3; Constant 3]);
		assert_equal (Some ["d", Variable "a"; "c", Variable "a"]) (unify [Variable "c"; Variable "d"] [Variable "a"; Variable "a"]);
		assert_equal None (unify [Constant 0] [Constant 1]);
		assert_equal (Some []) (unify [Constant 0] [Constant 0])

	in

	"Util" >::: [
		"unify" >:: testUnify
	]*)
