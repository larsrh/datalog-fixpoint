open List

open MyBat
open Types
open Util

let test =
	let open OUnit in

	let root = "tests/pos" in
	let f folder () =
		let toSet list = fold_right StringSet.add list StringSet.empty in
		let fixpoint =
			readFile (folder ^ Filename.dir_sep ^ "input") |>
			String.concat " " |>
			Lexer.parseString |>
			PosDatalog.fixpoint |>
			map PosDatalog.showClause in
		let output = readFile (folder ^ Filename.dir_sep ^ "output") in
		assert_equal (length fixpoint) (length output);
		assert_equal ~cmp:StringSet.equal (toSet output) (toSet fixpoint) in
	let case folder = folder >:: f (root ^ Filename.dir_sep ^ folder) in
	let cases = Sys.readdir root |> Array.to_list |> map case in
	"FullTest" >::: cases
