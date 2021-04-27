structure TypeCheckEvaluator = struct
	open AST
	fun typeMismatch (msg : string) : value = raise Fail msg
		
	fun evalExp (e : expn, env : environment) : value =
		case e of
			NumExp i					=>		IntVal i
			| BoolExp b					=>		BoolVal b
			| Fn (x, t1, t2, exp)		=>		evalFunDef(x, t1, t2, exp, env)
			| VarExp x					=>		envLookup(x, env)
			| negate exp				=>		evalNegate(exp, env)
			| BinExp (b, e1, e2)		=>		evalBinExp(b, e1, e2, env)
			| NOT exp					=>		evalNot(exp, env)
			| AppExp (f, exp)			=>		evalFunApp(f, exp, env)
			| LetExp (x, eval, exp)		=>		evalLetExp(x, eval, exp, env)
			| IfExp (b, e1, e2)			=>		evalIfExp(b, e1, e2, env)
			| Fun (f, x, t1, t2, exp)	=>		evalFunExp(f, x, t1, t2, exp, env)
	
	and
	
	evalBinExp(b : binop, e1 : expn, e2 : expn, env : environment) : value =
		let
			val v1 = evalExp(e1, env)
			val v2 = evalExp(e2, env)
		in
			case (b, v1, v2) of
				(PLUS, IntVal i1, IntVal i2)			=>		IntVal (i1 + i2)
				| (MINUS, IntVal i1, IntVal i2)			=>		IntVal (i1 - i2)
				| (TIMES, IntVal i1, IntVal i2)			=>		IntVal (i1 * i2)
				| (EQUALS, IntVal i1, IntVal i2)		=>		BoolVal (i1 = i2)
				| (LESSTHAN, IntVal i1, IntVal i2)		=>		BoolVal (i1 < i2)
				| (GREATERTHAN, IntVal i1, IntVal i2)	=>		BoolVal (i1 > i2)
				| (EQUALS, BoolVal i1, BoolVal i2)		=>		BoolVal (i1 = i2)
				| (IMPLIES, BoolVal i1, BoolVal i2)		=>		BoolVal (i2 orelse not i1)
				| (AND, BoolVal i1, BoolVal i2)			=>		BoolVal (i1 andalso i2)
				| (OR, BoolVal i1, BoolVal i2)			=>		BoolVal (i1 orelse i2)
				| (XOR, BoolVal i1, BoolVal i2)			=>		BoolVal ((i1 andalso i2) orelse (not i1 andalso not i2))
				| _										=>		typeMismatch("Type mismatch occurred in Binary Operator evaluation.\n")
		end
	
	and
	
	evalNegate(exp : expn, env : environment) : value =
		let
			val v = evalExp(exp, env)
		in
			case v of
				IntVal i	=>		IntVal (0 - i)
				| _			=>		typeMismatch("Type mismatch occurred in NEGATE Operator evaluation.\n")
		end
	
	and
	
	evalNot(exp : expn, env : environment) : value =
		let
			val v = evalExp(exp, env)
		in
			case v of
				BoolVal b	=>		BoolVal (not b)
				| _			=>		typeMismatch("Type mismatch occurred in NOT Operator evaluation.\n")
		end
	
	and
	
	evalLetExp(x : id, eval : expn, exp : expn, env : environment) : value =
		let
			val v = evalExp(eval, env)
		in
			evalExp(exp, envAdd (x, v, env))
		end
	
	and
	
	evalIfExp(b : expn, e1 : expn, e2 : expn, env : environment) : value =
		let
			val b1 = evalExp(b, env)
			val v1 = evalExp(e1, env)
			val v2 = evalExp(e2, env)
			fun ifthen(b : bool, i : value, j : value) : value = if b then i else j
		in
			case (b1, v1, v2) of
				(BoolVal cond, IntVal i1, IntVal i2)		=>		ifthen(cond, IntVal i1, IntVal i2)
				| (BoolVal cond, BoolVal i1, BoolVal i2)	=>		ifthen(cond, BoolVal i1, BoolVal i2)
				| (BoolVal cond, FunVal i1, FunVal i2)		=>		ifthen(cond, FunVal i1, FunVal i2)
				| _											=>		typeMismatch("Type mismatch occurred in IF-THEN-ELSE-FI evaluation.\n")
		end
	
	and
	
	evalFunApp(f : id, exp : expn, env : environment) : value =
		let
			val v1 = envLookup(f, env)
			val v2 = evalExp(exp, env)
		in
			case v1 of
				FunVal (x, t1, t2, e)	=>
					if sameType(v2, t1) then
						evalExp(e, envUpdate (x, v2, env))
					else
						typeMismatch("Type mismatch occurred in function evaluation.\n")
				| _						=>
					typeMismatch("Type mismatch occurred in function evaluation.\n")
		end
	
	and
	
	evalFunDef(x : id, t1 : typ, t2 : typ, exp : expn, env : environment) : value = FunVal (x, t1, t2, exp)
	
	and
	
	evalFunExp(f : id, x : id, t1 : typ, t2 : typ, exp : expn, env : environment) : value = FunVal (x, t1, t2, exp)
	
	and
	
	evalStatements(s : lines, env : environment) : unit =
		case s of
			statement S	=>
				let
					val output = evalExp(S, env)
				in
					case output of
						IntVal i				=>		(printExpression(S); print("\nOutput: "); print((Int.toString(i))^"\n\n"))
						| BoolVal b				=>		(printExpression(S); print("\nOutput: "); print(((BoolToString(b))^"\n\n")))
						| FunVal (x, a, b, c)	=>		(printExpression(S); print("\nOutput: "); print("fn "^x^": "); printType(a); print("->");
														printType(b); print("\t=>\t"); printExpression(c); print("\n\n"))
				end
			| statements (S, l)	=>
				let
					val output = evalExp(S, env)
				in
					case S of
					Fun(f, x, t1, t2, exp)	=>
					(case output of
						IntVal i				=>		(printExpression(S); print("\nOutput: "); print((Int.toString(i))^"\n\n");
														evalStatements(l, envAdd(f, FunVal (x, t1, t2, exp), env)))
						| BoolVal b				=>		(printExpression(S); print("\nOutput: "); print(((BoolToString(b))^"\n\n"));
														evalStatements(l, envAdd(f, FunVal (x, t1, t2, exp), env)))
						| FunVal (x, a, b, c)	=>		(printExpression(S); print("\nOutput: "); print("fn "^x^": "); printType(a);
														print("->"); printType(b); print(" => "); printExpression(c); print("\n\n");
														evalStatements(l, envAdd(f, FunVal (x, t1, t2, exp), env))))
					| _						=>
					(case output of
						IntVal i				=>		(printExpression(S); print("\nOutput: "); print((Int.toString(i))^"\n\n");
														evalStatements(l, env))
						| BoolVal b				=>		(printExpression(S); print("\nOutput: "); print(((BoolToString(b))^"\n\n"));
														evalStatements(l, env))
						| FunVal (x, a, b, c)	=>		(printExpression(S); print("\nOutput: "); print("fn "^x^": "); printType(a); print("->");
														printType(b); print(" => "); printExpression(c); print("\n\n");
														evalStatements(l, env)))
				end
	
	and
	
	sameType(v : value, t : typ) : bool =
		case v of
			IntVal _	 			=>
				(case t of
					INT				=>	true
					| _				=>	false)
			| BoolVal _				=>
				(case t of
					BOOL			=>	true
					| _				=>	false)
			| FunVal (x, a, b, c)	=>
				(case t of
					ARROW(t1, t2)	=>	(same(a, t1) andalso same(b, t2))
					| _				=>	false)
	
	and
	
	same(t1 : typ, t2 : typ) : bool =
		case t1 of
			INT	 			=>
				(case t2 of
					INT				=>	true
					| _				=>	false)
			| BOOL			=>
				(case t2 of
					BOOL			=>	true
					| _				=>	false)
			| ARROW(s1, s2)	=>
				(case t2 of
					ARROW(m1, m2)	=>	(same(m1, s1) andalso same(m2, s2))
					| _				=>	false)
	
	and
	
	printBinOp(s : binop) : unit =
		case s of
			IMPLIES			=>	print("IMPLIES")
			| AND			=>	print("AND")
			| OR			=>	print("OR")
			| XOR			=>	print("XOR")
			| PLUS			=>	print("PLUS")
			| MINUS			=>	print("MINUS")
			| TIMES			=>	print("TIMES")
			| EQUALS		=>	print("EQUALS")
			| LESSTHAN		=>	print("LESSTHAN")
			| GREATERTHAN	=>	print("GREATERTHAN")
	
	and
	
	printType(s : typ) : unit =
		case s of
			INT				=>	print("INT")
			| BOOL			=>	print("BOOL")
			| ARROW (a, b)	=>	(print("ARROW("); printType(a); print(", "); printType(b); print(")"))
	
	and
	
	printExpression(s : expn) : unit =
		case s of
			NumExp i					=>
					print("IntExp("^Int.toString(i)^")")
			| BoolExp b					=>
					print("BoolExp("^BoolToString(b)^")")
			| Fn (x, t1, t2, exp)		=>
					(print("FunDef("); print("\""^x^"\""); print(", "); printType(t1); print(", "); printType(t2); print(", ");
					printExpression(exp); print(")"))
			| VarExp x					=>
					print("VarExp(\""^x^"\")")
			| negate exp				=>
					(print("UnExp(NEGATE, "); printExpression(exp); print(")"))
			| BinExp (b, e1, e2)		=>
					(print("BinExp("); printBinOp(b); print(", "); printExpression(e1); print(", "); printExpression(e2); print(")"))
			| NOT exp					=>
					(print("UnExp(NOT, "); printExpression(exp); print(")"))
			| AppExp (f, exp)			=>
					(print("AppExp("); print("\""^f^"\""); print(", "); printExpression(exp); print(")"))
			| LetExp (x, eval, exp)		=>
					(print("LetExp("); print("\""^x^"\""); print(", "); printExpression(eval); print(", "); printExpression(exp); print(")"))
			| IfExp (b, e1, e2)			=>
					(print("IfExp("); printExpression(b); print(", "); printExpression(e1); print(", "); printExpression(e2); print(")"))
			| Fun (f, x, t1, t2, exp)	=>
					(print("FunExp("); print("\""^f^"\""); print(", "); print("\""^x^"\""); print(", "); printType(t1); print(", ");
					printType(t2); print(", "); printExpression(exp); print(")"))
	
	and
	
	printTree(s : lines) : unit =
		case s of
			statement S	=>
				(print("statement("); printExpression(S); print(")"))
			| statements (S, l)	=>
				(print("statements("); print("statement("); printExpression(S); print("), "); printTree(l); print(")"))
end