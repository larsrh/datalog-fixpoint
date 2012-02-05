open OUnit
open Batteries_uni
open List

open Util
open Datalog
open PosDatalog

let testUnify _ =
	assert_equal None (unify [Right 0; Right 0] [Right "a"; Right "b"]);
	assert_equal (Some [0, Right "a"]) (unify [Right 0; Right 0] [Right "a"; Right "a"]);
	assert_equal (Some [0, Left 3]) (unify [Right 0; Right 0] [Left 3; Left 3]);
	assert_equal (Some [1, Right "a"; 0, Right "a"]) (unify [Right 0; Right 1] [Right "a"; Right "a"]);
	assert_equal None (unify [Left 0] [Left 1]);
	assert_equal (Some []) (unify [Left 0] [Left 0])

let testPosDatalog _ =
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

let suite =
	"Tests" >::: [
		"unify" >:: testUnify;
		"posDatalog" >:: testPosDatalog
	]

let test _ =
	let unit _ = () in
	run_test_tt_main suite |> unit
