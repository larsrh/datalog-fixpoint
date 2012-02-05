open Datalog

include Datalog

type op = GEQ | GR

val mkNumber: int -> number
val mkPosConstraint: (int * var) list -> op -> int -> number constr option
val mkUpperBound: var -> bool -> int -> number constr
