open Types
open Util

val freshVar: stringSet -> var

val unify: 'a exp list -> 'a exp list -> 'a mapping option

val mergeEqualities: 'a equality list list -> 'a equality list option

val canonicalizeVars: 'a equality list -> 'a exp stringMap


val test: OUnit.test
