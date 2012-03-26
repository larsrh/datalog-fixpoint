(** Entry point for the test suite. Side effect: executes OUnit. *)

open OUnit

open MyBat
open Util

(**/**)

let suite =
	"Tests" >::: [
		Unification.test;
		PosDatalog.test;
		FullTest.test
	]

let _ =
	let unit _ = () in
	run_test_tt_main suite |> unit
