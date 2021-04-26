structure Tokens = Tokens
	type pos = int							(*keep track of the position of current token*)
	type svalue = Tokens.svalue
	type ('a,'b) token = ('a,'b) Tokens.token  
	type lexresult = (svalue, pos) token
	
	val col = ref 0
	val curr = ref 0
	val yylineno = ref 1
	val count = ref 0
	
	val eof = fn () => (print("]\n\nParse Tree Post-Order: \n["); Tokens.EOF(!yylineno, !col))
	val ret = fn (e : string) => if ((!count) = 0) then print("Scanned Stream of Tokens: \n["^e) else print(", "^e)
	val StringToBool = fn (s : string) => if (s = "TRUE") then true else false
	val allCaps = String.map Char.toUpper
	
	val keywords = [("fun",  Tokens.FUN), ("fn",  Tokens.FN), ("int",  Tokens.INT),("boolean",  Tokens.BOOL), ("PLUS",  Tokens.PLUS),
					("MINUS",  Tokens.MINUS), ("TIMES",  Tokens.TIMES), ("NEGATE",  Tokens.NEGATE), ("EQUALS",  Tokens.EQUALS),
					("LESSTHAN",  Tokens.LESSTHAN), ("GREATERTHAN",  Tokens.GREATERTHAN), ("AND",  Tokens.AND), ("OR",  Tokens.OR),
					("XOR",  Tokens.XOR), ("NOT",  Tokens.NOT), ("IMPLIES",  Tokens.IMPLIES), ("if",  Tokens.IF), ("then",  Tokens.THEN),
					("else",  Tokens.ELSE), ("fi",  Tokens.FI), ("let",  Tokens.LET), ("in",  Tokens.IN), ("end",  Tokens.END)]

	fun findKeywords (str:string) =
		if str = "TRUE" orelse str = "FALSE" then
			(ret("CONST \""^str^"\""); count := (!count) + 1; Tokens.CONST(StringToBool(str),!yylineno,!curr))
		else
			case List.find (fn (s, _) => s = str) keywords of 
				SOME (_, tk) => (ret(allCaps(str)^" \""^str^"\""); count := (!count) + 1; tk(!yylineno,!curr))
				| NONE => (ret("ID \""^str^"\""); count := (!count) + 1; Tokens.ID(str, !yylineno,!curr))

%%
%header (functor compilerLexFun(structure Tokens: compiler_TOKENS));
ws = [\ \t];
digit = [0-9];

%%
\n						=> (curr := !col; col := 0; yylineno := !yylineno + 1; lex());
{ws}+					=> (curr := !col; col := (!col) + size(yytext); lex());
"("						=> (curr := !col; col := (!col) + 1; ret("LPAREN \""^yytext^"\""); count := (!count) + 1; Tokens.LPAREN(!yylineno,!curr));
")"						=> (curr := !col; col := (!col) + 1; ret("RPAREN \""^yytext^"\""); count := (!count) + 1; Tokens.RPAREN(!yylineno,!curr));
";"						=> (curr := !col; col := (!col) + 1; ret("TERM \""^yytext^"\""); count := (!count) + 1; Tokens.TERM(!yylineno,!curr));
"="						=> (curr := !col; col := (!col) + 1; ret("ASSIGN \""^yytext^"\""); count := (!count) + 1; Tokens.ASSIGN(!yylineno,!curr));
":"						=> (curr := !col; col := (!col) + 1; ret("COLON \""^yytext^"\""); count := (!count) + 1; Tokens.COLON(!yylineno,!curr));
"->"					=> (curr := !col; col := (!col) + 2; ret("ARROW \""^yytext^"\""); count := (!count) + 1; Tokens.ARROW(!yylineno,!curr));
"=>"					=> (curr := !col; col := (!col) + 2; ret("DEF \""^yytext^"\""); count := (!count) + 1; Tokens.DEF(!yylineno,!curr));
[A-Za-z][A-Za-z0-9]*	=> (curr := !col; col := (!col) + size(yytext); findKeywords(yytext));
{digit}+				=> (curr := !col; col := (!col) + size(yytext); ret("NUM \""^yytext^"\""); count := (!count) + 1; Tokens.NUM(valOf (Int.fromString yytext),!yylineno,!curr));
.						=> (raise invalidTokenError("Unknown Token:"^Int.toString(!yylineno)^":"^Int.toString(!col)^":"^yytext));