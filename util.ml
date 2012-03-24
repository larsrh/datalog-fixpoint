open List

open MyBat

let swap (x, y) = y, x

let rec repeat x = function
| 0 -> []
| n -> x :: repeat x (n-1)

let rec groupBy f = function
| [] -> []
| x :: xs ->
	let res = groupBy f xs in
	let y = f x in
	let rec lookup elem = function
	| [] -> None
	| (x, y) :: xs when x = elem -> Some y
	| _ :: xs -> lookup elem xs in
	match lookup y res with
	| None -> (y, [x]) :: res
	| Some ys ->
		let removed = remove_assoc y res in
		(y, x :: ys) :: removed

let rec collect f = function
| [] -> []
| x :: xs ->
	match f x with
	| None -> collect f xs
	| Some y -> y :: collect f xs

let mapOption f = function
| Some y -> Some (f y)
| None -> None

let bindOption f = function
| Some y -> f y
| None -> None

let getOption = function
| Some y -> y
| None -> assert false

let rec foldLeftOption f acc = function
| [] -> acc
| x :: xs -> bindOption (fun y -> foldLeftOption f (f y x) xs) acc

let rec foldRightOption f list acc = match list with
| [] -> acc
| x :: xs -> bindOption (f x) (foldRightOption f xs acc)

let sequenceList list =
	let f x acc = mapOption (fun y -> y :: acc) x in
	Some [] |> foldRightOption f list

let actionToList action exn =
	let vals = ref [] in
	try
		while true do
			vals := action () :: !vals
		done;
		assert false
	with e when e = exn ->
		List.rev !vals
