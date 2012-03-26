(** Lexical analyzer for positive Datalog programs. Serves as an interface to
    the parser. *)

(** Reads [stdin] and yields a list of parsed {{!PosDatalog}positive Datalog}
    {{!Datalog.DatalogTypes.clause} clauses}. *)
val parseStdIn: unit -> PosDatalog.clause list

(** Reads a [string] and yields a list of parsed
    {{!PosDatalog}positive Datalog}
    {{!Datalog.DatalogTypes.clause} clauses}. *)
val parseString: string -> PosDatalog.clause list
