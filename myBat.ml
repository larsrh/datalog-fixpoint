open List

(** Utility functions. *)

(* From the 'Batteries' library, licensed under LGPL 2.1 (or later) *)
(* <https://github.com/ocaml-batteries-team/batteries-included> *)

let (|>) x f = f x
let (|-) f g x = g (f x)
let flip f x y = f y x

let cartesianProduct l1 l2 =
	concat (map (fun i -> map (fun j -> (i,j)) l2) l1)

let rec nCartesianProduct =	function
| [] -> [[]]
| [l] -> map (fun i -> [i]) l
| h :: t ->
	let rest = nCartesianProduct t in
	map (fun i -> map (fun r -> i :: r) rest) h |> concat
