open List

open MyBat

let _ =
	Lexer.parseStdIn () |>
	PosDatalog.fixpoint |>
	map PosDatalog.showClause |>
	iter print_endline
