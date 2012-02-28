type var = string

type 'a exp = Constant of 'a | Variable of var

type 'a result = Result of 'a | Tautology | Contradiction

module StringSet = Set.Make(String)

module StringMap = Map.Make(String)

type stringSet = StringSet.t
type 'a stringMap = 'a StringMap.t

type 'a equality = {
	number: 'a option;
	vars: stringSet
}

type 'a mapping = {
	assignment: 'a exp stringMap;
	equalities: 'a equality list
}
