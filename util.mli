open Datalog

val some: 'a -> 'a option

val lookup: 'a -> ('a * 'b) list -> 'b option

val swap: 'a * 'b -> 'b * 'a

val repeat: 'a -> int -> 'a list

val groupBy: ('a -> 'b) -> 'a list -> ('b * 'a list) list

val mapOption: ('a -> 'b) -> 'a option -> 'b option

val option: ('a -> 'b) -> 'b -> 'a option -> 'b

val getOption: 'a option -> 'a

val foldLeftOption: ('a -> 'b -> 'a option) -> 'a option -> 'b list -> 'a option

module StringSet: Set.S with type elt = string
module StringMap: Map.S with type key = string

type stringSet = StringSet.t
type 'a stringMap = 'a StringMap.t


val test: OUnit.test
