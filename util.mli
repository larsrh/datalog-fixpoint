val swap: 'a * 'b -> 'b * 'a

val repeat: 'a -> int -> 'a list

val groupBy: ('a -> 'b) -> 'a list -> ('b * 'a list) list

val collect: ('a -> 'b option) -> 'a list -> 'b list

val mapOption: ('a -> 'b) -> 'a option -> 'b option

val getOption: 'a option -> 'a

val foldLeftOption: ('a -> 'b -> 'a option) -> 'a option -> 'b list -> 'a option
