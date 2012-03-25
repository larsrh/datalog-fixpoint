open Datalog
open Types

include Interface with type number = int

val mkPosConstraint: (int * var) list -> bool -> int -> number constr result option
val mkUpperBound: var -> bool -> int -> number constr


val test: OUnit.test
