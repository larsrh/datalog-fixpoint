open Batteries_uni
open List

type ('a, 'b) either = Left of 'a | Right of 'b

let left x = Left x and right y = Right y

let either f g = function
| Left a -> f a
| Right b -> g b

let some f = Some f

let rec lookup elem = function
| [] -> None
| (x, y) :: xs when x = elem -> Some y
| _ :: xs -> lookup elem xs

let rec repeat x = function
| 0 -> []
| n -> x :: repeat x (n-1)

module M = Enum.WithMonad(Option.Monad)

let unify l1 l2 =
	let combined = combine l1 l2
	and f constrs = function
	| Left c1, Left c2 ->
		if c1 = c2
			then Some constrs
			else None
	| Left c, Right _ ->
		None
	| Right v, param ->
		match lookup v constrs with
		| None -> Some ((v, param) :: constrs)
		| Some binding when param = binding -> Some constrs
		| Some _ -> None in
	M.fold_monad f [] (enum combined)
