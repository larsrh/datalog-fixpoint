open OUnit

open MyBat
open Util

let suite =
	"Tests" >::: [
		Unification.test;
		Datalog.test;
		PosDatalog.test
	]

let _ =
	let unit _ = () in
	run_test_tt_main suite |> unit
