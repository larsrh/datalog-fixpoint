(** Limited kind of unification, where only {{!Types.exp}simple expressions}
    are allowed. *)

open Types
open Util

(** Generates a fresh identifier. This function has {e no} side-effects. It is
    guaranteed that [freshVar s] is not a member of [s]. *)
val freshVar: stringSet -> var

(** Unifies the head of a clause with a symbol occuring in the body of another
    clause. Note that this operation is {e not} commutative, as it does not
    perform a general unification.

    The result of this operation is a {{!Types.mapping}mapping}, i.e. a pair
    consisting of an assignment for the variables of the head, and a list of
    {{!Types.equality}equalities}, i.e. the variables of the body parameter
    which are determined to be equal.

    This representation may seem odd at first, but has been chosen for
    practical purposes.

    Example: The standard unification algorithm executed on the expression
    lists [[X, X, Y, 0]] and [[U, V, 1, V]] yields the answer
    [X = 0, Y = 1, U = V, V = 0]. [unify] produces the same output, but
    segregates the variables in the first list from those in the second list.

    The output in this example would be the assignment [X -> 0, Y -> 1] and the
    equalities [0 = U = V]. From that, it follows exactly which variables have
    to replaced by what in which clause.

    The resulting list of equalities is minimal. *)
val unify: 'a exp list -> 'a exp list -> 'a mapping option

(** Merges an arbitrary number of equality lists. The result is [Some y] iff
    no contradiction occurs, where [y] is minimal under the assumption that
    each input equality list is minimal. Otherwise, the behaviour is undefined.

    A list of equalities is minimal iff it does not contain any two equalities
    which share a common variable. *)
val mergeEqualities: 'a equality list list -> 'a equality list option

(** Produces an assignment from a minimal list of equalities. For each
    equality which contains a constant, the corresponding variables are mapped
    to that constant. In the other case, a fresh variable is generated and all
    variables in this equality are mapped to that fresh variable. *)
val canonicalizeVars: 'a equality list -> 'a exp stringMap


(**/**)

val test: OUnit.test
