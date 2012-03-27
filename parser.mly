%{
	open Types
	open Datalog
	open PosDatalog
	open MyBat
	open Util

	type item = Symbol of number symbol | Constraint of constr
%}
%token <string> IDENTIFIER
%token <int> NUMBER
%token LPAREN RPAREN DOT COMMA ARROW
%token TIMES PLUS MINUS LESS LEQ GREATER GEQ
%start clause
%type <PosDatalog.clause> clause
%%
clause:
  symbol DOT { {head = $1; syms = []; constraints = []} }
| symbol ARROW body DOT {
	let toSym = function
	| Symbol s -> Some s
	| _ -> None in
	let toConstr = function
	| Constraint c -> Some c
	| _ -> None in

	{ head = $1; syms = collect toSym $3; constraints = collect toConstr $3 }
}
;
body:
  item { [$1] }
| item COMMA body { $1 :: $3 }
;
item:
  constr { Constraint $1 }
| symbol { Symbol $1 }
;
constr:
  IDENTIFIER lt wholeNumber { mkUpperBound $1 $2 $3 }
| MINUS IDENTIFIER gt wholeNumber { mkUpperBound $2 $3 (-$4) }
| sum gt wholeNumber {
	match mkPosConstraint $1 $2 $3 with
	| Some (Result r) -> r
	| _ -> assert false
}
;
wholeNumber:
  NUMBER { $1 }
| MINUS NUMBER { -$2 }
;
gt:
  GEQ { true }
| GREATER { false }
;
lt:
  LEQ { true }
| LESS { false }
;
product:
  IDENTIFIER TIMES NUMBER { $3, $1 }
| NUMBER TIMES IDENTIFIER { $1, $3 }
| IDENTIFIER { 1, $1 }
;
sum:
  product { [$1] }
| product PLUS sum { $1 :: $3 }
;
symbol:
  IDENTIFIER { {rel = $1; params = [] } }
| IDENTIFIER LPAREN params RPAREN { {rel = $1; params = $3 } }
;
params:
  param { [$1] }
| param COMMA params { $1 :: $3 }
;
param:
  NUMBER { Constant $1 }
| IDENTIFIER { Variable $1 }
;
