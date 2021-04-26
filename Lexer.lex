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

%%
%header (functor compilerLexFun(structure Tokens: compiler_TOKENS));
alpha = [A-Za-z];
digit = [0-9];
ws = [\ \t];

%%
\n				=> (curr := !col; col := 0; yylineno := !yylineno + 1; lex());
{ws}+			=> (curr := !col; col := (!col) + size(yytext); lex());
"PLUS"			=> (curr := !col; col := (!col) + 4; ret("PLUS \""^yytext^"\""); count := (!count) + 1; Tokens.PLUS(!yylineno,!curr));
"MINUS"			=> (curr := !col; col := (!col) + 5; ret("MINUS \""^yytext^"\""); count := (!count) + 1; Tokens.MINUS(!yylineno,!curr));
"TIMES"			=> (curr := !col; col := (!col) + 5; ret("TIMES \""^yytext^"\""); count := (!count) + 1; Tokens.TIMES(!yylineno,!curr));
"NEGATE"		=> (curr := !col; col := (!col) + 6; ret("NEGATE \""^yytext^"\""); count := (!count) + 1; Tokens.NEGATE(!yylineno,!curr));
"EQUALS"		=> (curr := !col; col := (!col) + 6; ret("EQUALS \""^yytext^"\""); count := (!count) + 1; Tokens.EQUALS(!yylineno,!curr));
"LESSTHAN"		=> (curr := !col; col := (!col) + 8; ret("LESSTHAN \""^yytext^"\""); count := (!count) + 1; Tokens.LESSTHAN(!yylineno,!curr));
"GREATERTHAN"	=> (curr := !col; col := (!col) + 11; ret("GREATERTHAN \""^yytext^"\""); count := (!count) + 1; Tokens.GREATERTHAN(!yylineno,!curr));
"AND"			=> (curr := !col; col := (!col) + 3; ret("AND \""^yytext^"\""); count := (!count) + 1; Tokens.AND(!yylineno,!curr));
"OR"			=> (curr := !col; col := (!col) + 2; ret("OR \""^yytext^"\""); count := (!count) + 1; Tokens.OR(!yylineno,!curr));
"XOR"			=> (curr := !col; col := (!col) + 3; ret("XOR \""^yytext^"\""); count := (!count) + 1; Tokens.XOR(!yylineno,!curr));
"NOT"			=> (curr := !col; col := (!col) + 3; ret("NOT \""^yytext^"\""); count := (!count) + 1; Tokens.NOT(!yylineno,!curr));
"IMPLIES"		=> (curr := !col; col := (!col) + 7; ret("IMPLIES \""^yytext^"\""); count := (!count) + 1; Tokens.IMPLIES(!yylineno,!curr));
"if"			=> (curr := !col; col := (!col) + 2; ret("IF \""^yytext^"\""); count := (!count) + 1; Tokens.IF(!yylineno,!curr));
"then"			=> (curr := !col; col := (!col) + 4; ret("THEN \""^yytext^"\""); count := (!count) + 1; Tokens.THEN(!yylineno,!curr));
"else"			=> (curr := !col; col := (!col) + 4; ret("ELSE \""^yytext^"\""); count := (!count) + 1; Tokens.ELSE(!yylineno,!curr));
"fi"			=> (curr := !col; col := (!col) + 2; ret("FI \""^yytext^"\""); count := (!count) + 1; Tokens.FI(!yylineno,!curr));
"let"			=> (curr := !col; col := (!col) + 3; ret("LET \""^yytext^"\""); count := (!count) + 1; Tokens.LET(!yylineno,!curr));
"in"			=> (curr := !col; col := (!col) + 2; ret("IN \""^yytext^"\""); count := (!count) + 1; Tokens.IN(!yylineno,!curr));
"end"			=> (curr := !col; col := (!col) + 3; ret("END \""^yytext^"\""); count := (!count) + 1; Tokens.END(!yylineno,!curr));
"TRUE"|"FALSE"	=> (curr := !col; col := (!col) + size(yytext); ret("CONST \""^yytext^"\""); count := (!count) + 1; Tokens.CONST(StringToBool(yytext),!yylineno,!curr));
"("				=> (curr := !col; col := (!col) + 1; ret("LPAREN \""^yytext^"\""); count := (!count) + 1; Tokens.LPAREN(!yylineno,!curr));
")"				=> (curr := !col; col := (!col) + 1; ret("RPAREN \""^yytext^"\""); count := (!count) + 1; Tokens.RPAREN(!yylineno,!curr));
";"				=> (curr := !col; col := (!col) + 1; ret("TERM \""^yytext^"\""); count := (!count) + 1; Tokens.TERM(!yylineno,!curr));
"="				=> (curr := !col; col := (!col) + 1; ret("ASSIGN \""^yytext^"\""); count := (!count) + 1; Tokens.ASSIGN(!yylineno,!curr));
{digit}+		=> (curr := !col; col := (!col) + size(yytext); ret("NUM \""^yytext^"\""); count := (!count) + 1; Tokens.NUM(valOf (Int.fromString yytext),!yylineno,!curr));
{alpha}+		=> (curr := !col; col := (!col) + size(yytext); ret("ID \""^yytext^"\""); count := (!count) + 1; Tokens.ID(yytext,!yylineno,!curr));
.				=> (raise invalidTokenError("Unknown Token:"^Int.toString(!yylineno)^":"^Int.toString(!col)^":"^yytext));