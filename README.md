To scan and parse a file with path "filepath":
	1. Run "ml-lex Lexer.lex" on the shell/terminal.
	2. Run "ml-yacc Parser.yacc" on the shell/terminal.
	3. Run sml and type 'use "loader.sml";' in the sml command line.
	4. Run the command 'compile "filepath"' in the sml command line.
	5. The output is printed on the console.