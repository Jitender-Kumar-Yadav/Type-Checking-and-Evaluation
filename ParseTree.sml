fun PFun(ID1, ID2, typ1, typ2, formula1) =
	let
		val x1 = ID1()
		val x2 = ID2()
		val x3 = typ1()
		val x4 = typ2()
		val x5 = formula1()
	in
		print("FUN, ");
		print(x1^", ");
		print("(, ");
		print(x2^", ");
		print(":, ");
		print("), ");
		print(":, ");
		print("=>, ");
		print("line, ");
		AST.Fun(x1, x2, x3, x4, x5)
	end

fun PIf(formula1, line1, line2) =
	let
		val x1 = formula1()
		val x2 = line1()
		val x3 = line2()
		val x4 = typ2()
		val x5 = formula1()
	in
		print("FUN, ");
		print(x1^", ");
		print("(, ");
		print(x2^", ");
		print(":, ");
		print("), ");
		print(":, ");
		print("=>, ");
		print("line, ");
		AST.Fun(x1, x2, x3, x4, x5)
	end
	print("IF, "); val x1 = formula1; print("THEN, "); val x2 = line1; print("ELSE, "); val x3 = line2; print("FI, "); print("line, "); AST.IfExp(x1, x2, x3)



















