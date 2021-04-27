structure AST = struct
	type id = string
	
	datatype binop = IMPLIES | AND | OR | XOR | PLUS | MINUS | TIMES | EQUALS | LESSTHAN | GREATERTHAN
	
	datatype typ = INT | BOOL | ARROW of typ * typ
	
	datatype value = IntVal of int | BoolVal of bool | FunVal of id * typ * typ * expn
	and
	expn = NumExp of int
		| BoolExp of bool
		| Fn of id * typ * typ * expn
		| VarExp of id
		| negate of expn
		| BinExp of binop * expn * expn
		| NOT of expn
		| AppExp of id * expn
		| LetExp of id * expn * expn
		| IfExp of expn * expn * expn
		| Fun of id * id * typ * typ * expn
	
	datatype lines = statement of expn | statements of expn * lines
	
	type environment = (id * value) list
	
	fun envAdd (var:id, v:value, env:environment) =
		case List.find(fn (x, _) => x = var) env of
			SOME(x, v) => raise Fail ("Variable " ^ var ^ " already declared.")
			| NONE => (var,v)::env
	
	fun envUpdate (var:id, v:value , env : environment) =
		case List.find(fn (x, _) => x = var) env of
			SOME(x, v)	=> ((var,v)::(List.filter(fn y => not(y = (x, v))) env))
			| NONE		=> (var,v)::env
		
	
	fun envLookup (var:id, env:environment) =
		case List.find(fn (x, _) => x = var) env of
			SOME(x, v) => v
			| NONE => raise Fail ("Variable " ^ var ^ " not declared yet.")
end