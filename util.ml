open List

open MyBat
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

let rec groupBy f = function
| [] -> []
| x :: xs ->
	let res = groupBy f xs in
	let y = f x in
	match lookup y res with
	| None -> (y, [x]) :: res
	| Some ys ->
		let removed = remove_assoc y res in
		(y, x :: ys) :: removed

let mapOption f = function
| Some y -> f y |> some
| None -> None

let getOption = function
| Some y -> y
| None -> raise Not_found

let rec foldLeftOption f acc = function
| [] -> acc
| x :: xs ->
	match acc with
	| Some y -> foldLeftOption f (f y x) xs
	| None -> None


(** Tests **)

let test =
	let open OUnit in
	"Util" >::: [
	]
