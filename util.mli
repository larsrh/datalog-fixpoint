val swap: 'a * 'b -> 'b * 'a

val repeat: 'a -> int -> 'a list

val groupBy: ('a -> 'b) -> 'a list -> ('b * 'a list) list

val collect: ('a -> 'b option) -> 'a list -> 'b list

val mapOption: ('a -> 'b) -> 'a option -> 'b option

val bindOption: ('a -> 'b option) -> 'a option -> 'b option

val getOption: 'a option -> 'a

val foldLeftOption: ('a -> 'b -> 'a option) -> 'a option -> 'b list -> 'a option

val foldRightOption: ('a -> 'b -> 'b option) -> 'a list -> 'b option -> 'b option

val sequenceList: 'a option list -> 'a list option

val actionToList: (unit -> 'a) -> exn -> 'a list
