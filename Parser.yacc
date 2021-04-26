(*User  declarations*)
fun BoolToString(s : bool) = if s then "TRUE" else "FALSE";

%%
(*Declarations*)
%name compiler

%term
	ID of string
	| CONST of bool
	| NUM of int
	| AND | OR | XOR | IMPLIES | NOT
	| PLUS | MINUS | TIMES | NEGATE
	| EQUALS | LESSTHAN | GREATERTHAN
	| RPAREN | LPAREN | TERM | ASSIGN | EOF
	| IF | THEN | ELSE | FI | LET | IN | END

%nonterm START | program | statement | line | formula

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

%right IMPLIES
%left AND OR XOR EQUALS LESSTHAN GREATERTHAN
%right NOT
%left PLUS MINUS
%left TIMES
%right NEGATE

%start START

%verbose

%%
(*Rules*)
START:		program										(program1; print("START]\n"))
program:	line										(line1; print("program, "))
			| line TERM program							(line1; print("TERM, "); program1; print("program, "))
line:		formula										(formula1; print("line, "))
			| IF formula THEN line ELSE line FI			(print("IF, "); formula1; print("THEN, "); line1; print("ELSE, "); line2; print("FI, "); print("line, "))
			| LET ID ASSIGN formula IN line END			(print("LET, "); print(ID1^", "); print("=, "); formula1; print("IN, "); line1; print("END, "); print("line, "))
formula:	formula EQUALS formula						(formula1; print("EQUALS, "); formula2; print("formula, "))
			| formula LESSTHAN formula					(formula1; print("LESSTHAN, "); formula2; print("formula, "))
			| formula GREATERTHAN formula				(formula1; print("GREATERTHAN, "); formula2; print("formula, "))
			| formula IMPLIES formula					(formula1; print("IMPLIES, "); formula2; print("formula, "))
			| formula AND formula						(formula1; print("AND, "); formula2; print("formula, "))
			| formula OR formula						(formula1; print("OR, "); formula2; print("formula, "))
			| formula XOR formula						(formula1; print("XOR, "); formula2; print("formula, "))
			| NEGATE formula							(print("NEGATE, "); formula1; print("formula, "))
			| formula PLUS formula						(formula1; print("PLUS, "); formula2; print("formula, "))
			| formula MINUS formula						(formula1; print("MINUS, "); formula2; print("formula, "))
			| formula TIMES formula						(formula1; print("TIMES, "); formula2; print("formula, "))
			| NOT formula								(print("NOT, "); formula1; print("formula, "))
			| LPAREN formula RPAREN						(print("(, "); formula1; print("), "); print("formula, "))
			| CONST										(print(BoolToString(CONST1)^", "); print("formula, "))
			| NUM										(print(Int.toString(NUM1)^", "); print("formula, "))
			| ID										(print(ID1^", "); print("formula, "))