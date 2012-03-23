open List

open Types
open MyBat
open Util

let freshVar set = StringSet.max_elt set ^ "_"

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
	let constant_constant c1 c2 = None
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

	if e1 = e2
		then unchanged
		else match e1, e2 with
			| Constant c1, Constant c2 -> constant_constant c1 c2
			| Constant c, Variable v -> constant_variable c v
			| Variable v, Constant c -> constant_variable c v
			| Variable v1, Variable v2 -> variable_variable v1 v2

let unify l1 l2 =
	let combined = combine l1 l2 in
	let f mapping =
		let addEq e1 e2 = addEquality mapping.equalities e1 e2 |> mapOption (fun eqs -> {mapping with equalities = eqs}) in
		let addAss v e = Some {mapping with assignment = StringMap.add v e mapping.assignment} in
		function
		| Constant c1, Constant c2 ->
			if c1 = c2
				then Some mapping
				else None
		| Variable v, e ->
			if StringMap.mem v mapping.assignment
				then addEq e (StringMap.find v mapping.assignment)
				else addAss v e
		| c, v ->
			addEq c v
	in

	foldLeftOption f (Some {assignment = StringMap.empty; equalities = []}) combined

let rec mergeEqualities = function
| [] -> Some []
| eqs :: eqss ->
	let rec f acc = function
	| [] -> Some acc
	| [_] -> Some acc
	| x :: y :: xs -> match addEquality acc x y with
		| None -> None
		| Some res -> y :: xs |> f res
	in

	let mergeOne acc eq =
		let vars = StringSet.elements eq.vars |> map (fun v -> Variable v) in
		let number = (match eq.number with | None -> [] | Some n -> [Constant n]) in
		vars @ number |> f acc
	in

	let mergeAll acc = foldLeftOption mergeOne acc eqs in
	mergeEqualities eqss |> mergeAll

let canonicalizeVars eqs =
	let open StringSet in
	let vars =
		let f acc eq = union acc eq.vars in
		fold_left f empty eqs in

	let canonical (map, vars) eq =
		let addAll map keys binding = fold_right (fun v -> StringMap.add v binding) keys map in
		match eq.number with
		| Some n ->
			Constant n |> addAll map (elements eq.vars), vars
		| None ->
			let fresh = freshVar vars in
			Variable fresh |> addAll map (elements eq.vars), add fresh vars
	in

	fold_left canonical (StringMap.empty, vars) eqs |> fst


let test = 
	let open OUnit in

	let testAddEquality _ =
		assert_equal (Some [{number = Some 3; vars = StringSet.singleton "x"}]) (addEquality [{number = None; vars = StringSet.singleton "x"}] (Constant 3) (Variable "x")); 
		assert_equal (Some [{number = Some 3; vars = StringSet.singleton "x"}]) (addEquality [] (Constant 3) (Variable "x"));
		assert_equal (Some [{number = None; vars = fold_right StringSet.add ["y"; "x"] StringSet.empty}]) (addEquality [{number = None; vars = StringSet.singleton "x"}] (Variable "y") (Variable "x"));
		assert_equal (Some [{number = None; vars = fold_right StringSet.add ["x"; "y"] StringSet.empty}]) (addEquality [{number = None; vars = StringSet.singleton "x"}; {number = None; vars = StringSet.singleton "y"}] (Variable "y") (Variable "x"))
	in

	let testMergeEqualities _ =
		assert_equal None (mergeEqualities [[{number = Some 3; vars = StringSet.singleton "y"}; {number = Some 4; vars = StringSet.singleton "x"}]; [{number = None; vars = fold_right StringSet.add ["x"; "y"] StringSet.empty}]]);
		assert_equal (Some [{number = Some 3; vars = fold_right StringSet.add["x"; "y"] StringSet.empty}]) (mergeEqualities [[{number = Some 3; vars = StringSet.singleton "y"}; {number = Some 3; vars = StringSet.singleton "x"}]; [{number = None; vars = fold_right StringSet.add ["x"; "y"] StringSet.empty}]])
	in

	let testCanonicalizeVars _ =
		assert_equal ~cmp:(StringMap.equal (=)) (fold_right2 StringMap.add ["x"; "y"; "z"] [Variable "z_"; Variable "z_"; Constant 3] StringMap.empty) (canonicalizeVars [{number = None; vars = fold_right StringSet.add ["x"; "y"] StringSet.empty}; {number = Some 3; vars = StringSet.singleton "z"}]);
		assert_equal ~cmp:(StringMap.equal (=)) (fold_right2 StringMap.add ["u"; "v"] [Constant 0; Constant 0] StringMap.empty) (canonicalizeVars [{number = Some 0; vars = fold_right StringSet.add ["v"; "u"] StringSet.empty}])
	in

	let testUnify _ =
		assert_equal (Some {assignment = StringMap.singleton "a" (Variable "a"); equalities = [{number = None; vars = fold_right StringSet.add ["b"; "a"] StringSet.empty}]}) (unify [Variable "a"; Variable "a"] [Variable "a"; Variable "b"]);
		assert_equal (Some {assignment = StringMap.singleton "b" (Variable "a"); equalities = []}) (unify [Variable "b"; Variable "b"] [Variable "a"; Variable "a"]);
		assert_equal (Some {assignment = StringMap.singleton "a" (Constant 3); equalities = []}) (unify [Variable "a"; Variable "a"] [Constant 3; Constant 3]);
		assert_equal None (unify [Constant 0] [Constant 1]);
		assert_equal (Some {assignment = StringMap.empty; equalities = []}) (unify [Constant 0] [Constant 0]);
		assert_equal (Some {assignment = fold_right2 StringMap.add ["y"; "x"] [Constant 1; Variable "u"] StringMap.empty; equalities = [{number = Some 0; vars = fold_right StringSet.add ["v"; "u"] StringSet.empty}]}) (unify [Variable "x"; Variable "x"; Variable "y"; Constant 0] [Variable "u"; Variable "v"; Constant 1; Variable "v"])
	in

	"Unification" >::: [
		"addEquality" >:: testAddEquality;
		"mergeEqualities" >:: testMergeEqualities;
		"canonicalizeVars" >:: testCanonicalizeVars;
		"unify" >:: testUnify
	]
