open Datalog
open Util

val freshVar: stringSet -> var

type 'a equality = {
	number: 'a option;
	vars: stringSet
}

type 'a mapping = {
	assignment: 'a exp stringMap;
	equalities: 'a equality list
}

val unify: 'a exp list -> 'a exp list -> (var * 'a exp) list option


val test: OUnit.test
