(** Limited kind of unification, where only {{!Types.exp}simple expressions}
    are allowed. *)

open Types
open Util

(** Generates a fresh identifier. This function has {e no} side-effects. It is
    guaranteed that [freshVar s] is not a member of [s]. *)
val freshVar: stringSet -> var

val unify: 'a exp list -> 'a exp list -> 'a mapping option

val mergeEqualities: 'a equality list list -> 'a equality list option

val canonicalizeVars: 'a equality list -> 'a exp stringMap


(**/**)

val test: OUnit.test
