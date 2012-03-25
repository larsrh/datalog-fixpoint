{
	open Parser
	open MyBat
	exception Eof
}

rule token = parse
| [' ' '\t' '\n'] { token lexbuf }
| ['0' - '9']+ as num { NUMBER(int_of_string num) }
| ['a' - 'z' 'A' - 'Z' '_']+ as id { IDENTIFIER(id) }
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
	let parse lexbuf =
		let f _ = Parser.clause token lexbuf in
		Util.actionToList f Eof

	let parseStdIn () =
		Lexing.from_channel stdin |> parse

	let parseString str =
		Lexing.from_string str |> parse
}
