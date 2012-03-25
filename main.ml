(** Entry point for the program. Side effect: calls {!main}. *)

open List

open MyBat

(** Reads a database from [stdin], computes the fixpoint and pretty prints the
    result to [stdout]. *)
let main () =
	Lexer.parseStdIn () |>
	PosDatalog.fixpoint |>
	map PosDatalog.showClause |>
	iter print_endline

let _ = main ()
