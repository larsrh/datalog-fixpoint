open Batteries_uni
open List

open Datalog

let some f = Some f

let rec lookup elem = function
| [] -> None
| (x, y) :: xs when x = elem -> Some y
| _ :: xs -> lookup elem xs

let swap (x, y) = y, x

let rec repeat x = function
| 0 -> []
| n -> x :: repeat x (n-1)

module M = Enum.WithMonad(Option.Monad)

let unify l1 l2 =
	let combined = combine l1 l2
	and f constrs = function
	| Constant c1, Constant c2 ->
		if c1 = c2
			then Some constrs
			else None
	| Constant c, Variable _ ->
		None
	| Variable v, param ->
		match lookup v constrs with
		| None -> Some ((v, param) :: constrs)
		| Some binding when param = binding -> Some constrs
		| Some _ -> None in
	M.fold_monad f [] (enum combined)


(** Tests **)

let test =
	let open OUnit in

	let testUnify _ =
		assert_equal None (unify [Variable "a"; Variable "a"] [Variable "a"; Variable "b"]);
		assert_equal (Some ["b", Variable "a"]) (unify [Variable "b"; Variable "b"] [Variable "a"; Variable "a"]);
		assert_equal (Some ["a", Constant 3]) (unify [Variable "a"; Variable "a"] [Constant 3; Constant 3]);
		assert_equal (Some ["d", Variable "a"; "c", Variable "a"]) (unify [Variable "c"; Variable "d"] [Variable "a"; Variable "a"]);
		assert_equal None (unify [Constant 0] [Constant 1]);
		assert_equal (Some []) (unify [Constant 0] [Constant 0])

	in

	"Util" >::: [
		"unify" >:: testUnify
	]
