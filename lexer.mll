{
	open Parser
	exception Eof
}

rule token = parse
| [' ' '\t' '\n'] { token lexbuf }
| ['0' - '9']+ as num { NUMBER(int_of_string num) }
| ['a' - 'z' 'A' - 'Z']+ as id { IDENTIFIER(id) }
| '(' { LPAREN }
| ')' { RPAREN }
| '.' { DOT }
| ',' { COMMA }
| ":-" { ARROW }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '<' { LESS }
| "<=" { LEQ }
| '>' { GREATER }
| ">=" { GEQ }
| eof { raise Eof }

{
	let parse () =
		let lines = ref [] in
		try
			let lexbuf = Lexing.from_channel stdin in
			while true do
				let line = Parser.clause token lexbuf in
				lines := line :: !lines
			done;
			[] (* dummy *)
		with Eof ->
			!lines
}
