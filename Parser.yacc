(*User  declarations*)

%%
(*Declarations*)
%name compiler

%term
	ID of string
	| CONST of bool
	| NUM of int
	| INT | BOOL
	| AND | OR | XOR | IMPLIES | NOT
	| PLUS | MINUS | TIMES | NEGATE
	| EQUALS | LESSTHAN | GREATERTHAN
	| RPAREN | LPAREN | TERM | ASSIGN | EOF
	| FUN | FN | COLON | DEF | ARROW
	| IF | THEN | ELSE | FI | LET | IN | END

%nonterm
	START of AST.lines | program of AST.lines | line of AST.expn
	| formula of AST.expn | typ of AST.typ

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

%right DEF
%right IMPLIES
%left AND OR XOR EQUALS LESSTHAN GREATERTHAN
%right NOT
%left PLUS MINUS
%left TIMES
%right NEGATE
%left ARROW

%start START

%verbose

%%
(*Rules*)
START:		program										(program1)

program:	line										(AST.statement(line1))
			| line TERM program							(AST.statements(line1, program1))

line:		formula										(formula1)
			| FUN ID LPAREN ID COLON typ RPAREN
			COLON typ DEF formula						(AST.Fun(ID1, ID2, typ1, typ2, formula1))
			| IF formula THEN line ELSE line FI			(AST.IfExp(formula1, line1, line2))
			| LET ID ASSIGN formula IN line END			(AST.LetExp(ID1, formula1, line1))

formula:	FN LPAREN ID COLON typ RPAREN
			COLON typ DEF formula						(AST.Fn(ID1, typ1, typ2, formula1))
			| formula EQUALS formula					(AST.BinExp(AST.EQUALS, formula1, formula2))
			| formula LESSTHAN formula					(AST.BinExp(AST.LESSTHAN, formula1, formula2))
			| formula GREATERTHAN formula				(AST.BinExp(AST.GREATERTHAN, formula1, formula2))
			| formula IMPLIES formula					(AST.BinExp(AST.IMPLIES, formula1, formula2))
			| formula AND formula						(AST.BinExp(AST.AND, formula1, formula2))
			| formula OR formula						(AST.BinExp(AST.OR, formula1, formula2))
			| formula XOR formula						(AST.BinExp(AST.XOR, formula1, formula2))
			| NEGATE formula							(AST.negate(formula1))
			| formula PLUS formula						(AST.BinExp(AST.PLUS, formula1, formula2))
			| formula MINUS formula						(AST.BinExp(AST.MINUS, formula1, formula2))
			| formula TIMES formula						(AST.BinExp(AST.TIMES, formula1, formula2))
			| NOT formula								(AST.NOT(formula1))
			| LPAREN formula RPAREN						(formula1)
			| LPAREN ID formula RPAREN					(AST.AppExp(ID1, formula1))
			| CONST										(AST.BoolExp(CONST1))
			| NUM										(AST.NumExp(NUM1))
			| ID										(AST.VarExp(ID1))

typ:		INT											(AST.INT)
			| BOOL										(AST.BOOL)
			| typ ARROW typ								(AST.ARROW(typ1, typ2))
			| LPAREN typ RPAREN							(typ1)