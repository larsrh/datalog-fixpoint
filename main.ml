open List

open MyBat

(** Entry point for the program. Reads a database from [stdin], computes and
    pretty prints the fixpoint. *)

let _ =
	Lexer.parseStdIn () |>
	PosDatalog.fixpoint |>
	map PosDatalog.showClause |>
	iter print_endline
