open List

open MyBat
open Types

module Int = struct
	type number = int

	let showNumber = string_of_int
end

module LinearConstraint = struct
	type 'a linearConstraint = (var * 'a) list

	let showLinearConstr f zero constr =
		let show (v, n) = f n ^ "*" ^ v in
		if length constr > 0
			then map show constr |> String.concat " + "
			else f zero

	let linearConstrVars constr = fold_right StringSet.add (map fst constr) StringSet.empty
end
