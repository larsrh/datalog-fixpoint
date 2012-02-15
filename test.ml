open OUnit
open Batteries_uni
open List

open Util

let testUnify _ =
	assert_equal None (unify [Right 0; Right 0] [Right "a"; Right "b"]);
	assert_equal (Some [0, Right "a"]) (unify [Right 0; Right 0] [Right "a"; Right "a"]);
	assert_equal (Some [0, Left 3]) (unify [Right 0; Right 0] [Left 3; Left 3]);
	assert_equal (Some [1, Right "a"; 0, Right "a"]) (unify [Right 0; Right 1] [Right "a"; Right "a"]);
	assert_equal None (unify [Left 0] [Left 1]);
	assert_equal (Some []) (unify [Left 0] [Left 0])

let suite =
	"Tests" >::: [
		"Util" >::: [
			"unify" >:: testUnify
		];
		PosDatalog.test
	]

let test _ =
	let unit _ = () in
	run_test_tt_main suite |> unit
