open Datalog
open Types

include Datalog with type number = int

type op = GEQ | GR

val mkPosConstraint: (int * var) list -> op -> int -> number constr option
val mkUpperBound: var -> bool -> int -> number constr


val test: OUnit.test
