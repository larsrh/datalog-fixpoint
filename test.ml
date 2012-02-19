open OUnit

open MyBat
open Util

let suite =
	"Tests" >::: [
		Util.test;
		PosDatalog.test
	]

let test _ =
	let unit _ = () in
	run_test_tt_main suite |> unit
