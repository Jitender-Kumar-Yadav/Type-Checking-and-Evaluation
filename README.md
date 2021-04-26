To scan and parse a file with path "filepath":
	1. Run "ml-lex Lexer.lex"
	2. Run "ml-yacc Parser.yacc"
	3. Run sml and tyep 'use "loader.sml";' in the sml command line.
	4. Run the command 'scanParse "filepath"' in the sml command line.
	5. The output is printed on the console.