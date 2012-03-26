(** Miscellaneous utility functions. *)

open Types

(** [groupBy f xs] groups the elements of [xs]. An element [x] is in the group
    [f x]. A group is a pair of a key [k] and a list [ys] such that for every
    element [y] of [ys] the equality [k = f y] holds. *)
val groupBy: ('a -> 'b) -> 'a list -> ('b * 'a list) list

(** Applies the specified mapping, removes all [None] elements, and drops the
    [Some] constructor of the remaining elements. *)
val collect: ('a -> 'b option) -> 'a list -> 'b list

(** Applies a mapping to an optional value. *)
val mapOption: ('a -> 'b) -> 'a option -> 'b option

(** Applies a function monadically to an optional value. The result of
    [bindOption f x] equals [y] iff [x = Some z] and [y = f z] for some [z], or
    else [None]. *)
val bindOption: ('a -> 'b option) -> 'a option -> 'b option

(** Unsafely drops the [Some] constructor of the argument. Raises an assertion
    failure if the argument is [None]. *)
val getOption: 'a option -> 'a

(** Left fold using the [option] monad. Repeatedly applies the given function
    and returns immediately when an intermediate result is [None]. *)
val foldLeftOption: ('a -> 'b -> 'a option) -> 'a option -> 'b list -> 'a option

(** Right fold using the [option] monad. See {!foldLeftOption}. *)
val foldRightOption: ('a -> 'b -> 'b option) -> 'a list -> 'b option -> 'b option

(** Traverse a [list] using the [option] applicative functor. The result of
    [sequenceList xs] is [Some ys] iff all elements [x] in [xs] are not
    [None]. *)
val sequenceList: 'a option list -> 'a list option

(** Remove all tautologies and return [Some ys] iff the input list did not
    contain contradictions. *)
val getResults: 'a result list -> 'a list option

(** Repeatedly calls the function until the specified exception is raised.
    Returns all results of the function in a list. *)
val actionToList: (unit -> 'a) -> exn -> 'a list

(** Reads the contents of a file line by line. *)
val readFile: string -> string list
