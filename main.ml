open List

open MyBat

let _ =
	Lexer.parse () |>
	PosDatalog.fixpoint |>
	map PosDatalog.showClause |>
	iter print_endline
