open OUnit
open Batteries_uni

let suite =
	"Tests" >::: [
		Util.test;
		PosDatalog.test
	]

let test _ =
	let unit _ = () in
	run_test_tt_main suite |> unit
