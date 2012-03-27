(** {!Datalog} instance for positive Datalog. *)

open Datalog
open Types

include Interface with type number = int

(** Create a positive constraint. The result will be [None] iff, after
    simplification, a coefficient is less than zero.

    Example: [mkPosConstraint [1, "x"; 2, "y"] true 2] translates to
    [x+2y >= 2] *)
val mkPosConstraint: (int * var) list -> bool -> int -> constr result option

(** Create an upper bound constraint. [mkUpperBound v incl b] means [v < b] iff
    [incl] is false, otherwise [v <= b].*)
val mkUpperBound: var -> bool -> int -> constr


(**/**)

val test: OUnit.test
