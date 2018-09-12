grammar mipl ;


@members{
    public void tprint(String token, String lexeme)
    {
        System.out.println("TOKEN: " + token + "    LEXEME: " + lexeme);
    }

    public void prRule(String lhs, String rhs)
    {
        System.out.println(lhs + " -> " + rhs);
    }
  
}


/* PARSER */
n_START:        n_PROG 
                    { 
                        prRule("n_START", "n_PROG"); 
                    } 
                ;
n_ADDOP         : T_PLUS
                    {
                    	prRule("n_ADDOP", "T_PLUS");
                        tprint("T_PLUS", $T_PLUS.text);
                    }
                | T_MINUS
                    {
                    	prRule("n_ADDOP", "T_MINUS");
                        tprint("T_MINUS", $T_MINUS.text);
                    }
                | T_OR
                    {
                    	prRule("n_ADDOP", "T_OR");
                        tprint("T_OR", $T_OR.text);
                    }
                ;
n_ADDOPLST      : /* epsilon */
                    {
                    	prRule("n_ADDOPLST", "epsilon");
                    }
                | n_ADDOP n_TERM n_ADDOPLST
                    {
                    	prRule("n_ADDOPLST", "n_ADDOP n_TERM n_ADDOPLST");
                    }
                ;
n_ARRAY         : T_ARRAY T_LBRACK n_IDXRANGE T_RBRACK T_OF n_SIMPLE
                    {
                    	prRule("n_ARRAY", "T_ARRAY T_LBRACK n_IDXRANGE T_RBRACK T_OF n_SIMPLE");
                        tprint("T_ARRAY", $T_ARRAY.text);
                        tprint("T_LBRACK", $T_LBRACK.text);
                        tprint("T_RBRACK", $T_RBRACK.text);
                        tprint("T_OF", $T_OF.text);
                    }
                ;
n_ARRAYVAR      : n_ENTIREVAR
                    {
                    	prRule("n_ARRAYVAR", "n_ENTIREVAR");
                    }
                ;
n_ASSIGN        : n_VARIABLE T_ASSIGN n_EXPR
                    {
                    	prRule("n_ASSIGN", "n_VARIABLE T_ASSIGN n_EXPR");
                        tprint("T_ASSIGN", $T_ASSIGN.text);
                    }
                ;
n_BLOCK         : n_VARDECPART n_PROCDECPART n_STMTPART
                    {
                    	prRule("n_BLOCK", "n_VARDECPART n_PROCDECPART n_STMTPART");
                    }
                ;
n_BOOLCONST     : T_TRUE
                    {
                    	prRule("n_BOOLCONST", "T_TRUE");
                        tprint("T_TRUE", $T_TRUE.text);
                    }
                | T_FALSE
                    {
                    	prRule("n_BOOLCONST", "T_FALSE");
                        tprint("T_FALSE", $T_FALSE.text);
                    }
                ;
n_COMPOUND      : T_BEGIN n_STMT n_STMTLST T_END
                    {
                    	prRule("n_COMPOUND", "T_BEGIN n_STMT n_STMTLST T_END");
                        tprint("T_BEGIN", $T_BEGIN.text);
                        tprint("T_END", $T_END.text);
                    }
                ;
n_CONDITION     : T_IF n_EXPR T_THEN n_STMT
                    {
                    	prRule("n_CONDITION", "T_IF n_EXPR T_THEN n_STMT");
                        tprint("T_IF", $T_IF.text);
                        tprint("T_THEN", $T_THEN.text);
                    }
                | T_IF n_EXPR T_THEN n_STMT T_ELSE n_STMT
                    {
                    	prRule("n_CONDITION", "T_IF n_EXPR T_THEN n_STMT T_ELSE n_STMT");
                        tprint("T_IF", $T_IF.text);
                        tprint("T_THEN", $T_THEN.text);
                        tprint("T_ELSE", $T_ELSE.text);
                    }
                ;
n_CONST         : n_INTCONST
                    {
                    	prRule("n_CONST", "n_INTCONST");
                    }
                | T_CHARCONST
                    {
                    	prRule("n_CONST", "T_CHARCONST");
                        tprint("T_CHARCONST", $T_CHARCONST.text);
                    }
                | n_BOOLCONST
                    {
                    	prRule("n_CONST", "n_BOOLCONST");
                    }
                ;
n_ENTIREVAR     : n_VARIDENT
                    {
                    	prRule("n_ENTIREVAR", "n_VARIDENT");
                    }
                ;
n_EXPR          : n_SIMPLEEXPR
                    {
                    	prRule("n_EXPR", "n_SIMPLEEXPR");
                    }
                | n_SIMPLEEXPR n_RELOP n_SIMPLEEXPR
                    {
                    	prRule("n_EXPR", "n_SIMPLEEXPR n_RELOP n_SIMPLEEXPR");
                    }
                ;
n_FACTOR        : n_SIGN n_VARIABLE
                    {
                    	prRule("n_FACTOR", "n_SIGN n_VARIABLE");
                    }
                | n_CONST
                    {
                    	prRule("n_FACTOR", "n_CONST");
                    }
                | T_LPAREN n_EXPR T_RPAREN
                    {
                    	prRule("n_FACTOR", "T_LPAREN n_EXPR T_RPAREN");
                        tprint("T_LPAREN", $T_LPAREN.text);
                        tprint("T_RPAREN", $T_RPAREN.text);
                    }
                | T_NOT n_FACTOR
                    {
                    	prRule("n_FACTOR", "T_NOT n_FACTOR");
                        tprint("T_NOT", $T_NOT.text);
                    }
                ;
n_IDENT         : T_IDENT
                    {
                    	prRule("n_IDENT", "T_IDENT");
                        tprint("T_IDENT", $T_IDENT.text);
                    }
                ;
n_IDENTLST      : /* epsilon */
                    {
                    	prRule("n_IDENTLST", "epsilon");
                    }
                | T_COMMA n_IDENT n_IDENTLST
                    {
                    	prRule("n_IDENTLST", "T_COMMA n_IDENT n_IDENTLST");
                        tprint("T_COMMA", $T_COMMA.text);
                    }
                ;
n_IDX           : n_INTCONST
                    {
                    	prRule("n_IDX", "n_INTCONST");
                    }
                ;
n_IDXRANGE      : n_IDX T_DOTDOT n_IDX
                    {
                    	prRule("n_IDXRANGE", "n_IDX T_DOTDOT n_IDX");
                        tprint("T_DOTDOT", $T_DOTDOT.text);
                    }
                ;
n_IDXVAR        : n_ARRAYVAR T_LBRACK n_EXPR T_RBRACK
                    {
                    	prRule("n_IDXVAR", "n_ARRAYVAR T_LBRACK n_EXPR T_RBRACK");
                        tprint("T_LBRACK", $T_LBRACK.text);
                        tprint("T_RBRACK", $T_RBRACK.text);
                    }
                ;
n_INPUTLST      : /* epsilon */
                    {
                    	prRule("n_INPUTLST", "epsilon");
                    }
                | T_COMMA n_INPUTVAR n_INPUTLST
                    {
                    	prRule("n_INPUTLST", "T_COMMA n_INPUTVAR n_INPUTLST");
                        tprint("T_COMMA", $T_COMMA.text);
                    }
                ;
n_INPUTVAR      : n_VARIABLE
                    {
                    	prRule("n_INPUTVAR", "n_VARIABLE");
                    }
                ;
n_INTCONST      : n_SIGN T_INTCONST
                    {
                    	prRule("n_INTCONST", "n_SIGN T_INTCONST");
                        tprint("T_INTCONST", $T_INTCONST.text);
                    }
                ;
n_MULTOP        : T_MULT
                    {
                    	prRule("n_MULTOP", "T_MULT");
                        tprint("T_MULT", $T_MULT.text);
                    }
                | T_DIV
                    {
                    	prRule("n_MULTOP", "T_DIV");
                        tprint("T_DIV", $T_DIV.text);
                    }
                | T_AND
                    {
                    	prRule("n_MULTOP", "T_AND");
                        tprint("T_AND", $T_AND.text);
                    }
                ;
n_MULTOPLST     : /* epsilon */
                    {
                    	prRule("n_MULTOPLST", "epsilon");
                    }
                | n_MULTOP n_FACTOR n_MULTOPLST
                    {
                    	prRule("n_MULTOPLST", "n_MULTOP n_FACTOR n_MULTOPLST");
                    }
                ;
n_OUTPUT        : n_EXPR
                    {
                    	prRule("n_OUTPUT", "n_EXPR");
                    }
                ;
n_OUTPUTLST     : /* epsilon */
                    {
                    	prRule("n_OUTPUTLST", "epsilon");
                    }
                | T_COMMA n_OUTPUT n_OUTPUTLST
                    {
                    	prRule("n_OUTPUTLST", "T_COMMA n_OUTPUT n_OUTPUTLST");
                        tprint("T_COMMA", $T_COMMA.text);
                    }
                ;
n_PROCDEC       : n_PROCHDR n_BLOCK
                    {
                    	prRule("n_PROCDEC", "n_PROCHDR n_BLOCK");
                    }
                ;
n_PROCHDR       : T_PROC T_IDENT T_SCOLON
                    {
                    	prRule("n_PROCHDR", "T_PROC T_IDENT T_SCOLON");
                        tprint("T_PROC", $T_PROC.text);
                        tprint("T_IDENT", $T_IDENT.text);
                        tprint("T_SCOLON", $T_SCOLON.text);
                    }
                ;
n_PROCDECPART   : /* epsilon */
                    {
                    	prRule("n_PROCDECPART", "epsilon");
                    }
                | n_PROCDEC T_SCOLON n_PROCDECPART
                    {
                    	prRule("n_PROCDECPART", "n_PROCDEC T_SCOLON n_PROCDECPART");
                        tprint("T_SCOLON", $T_SCOLON.text);
                    }
                ;
n_PROCIDENT     : T_IDENT
                    {
                    	prRule("n_PROCIDENT", "T_IDENT");
                        tprint("T_IDENT", $T_IDENT.text);
                    }
                ;
n_PROCSTMT      : n_PROCIDENT
                    {
                    	prRule("n_PROCSTMT", "n_PROCIDENT");
                    }
                ;
n_PROG          : n_PROGLBL T_IDENT T_SCOLON n_BLOCK T_DOT
                    {
                    	prRule("n_PROG", "n_PROGLBL T_IDENT T_SCOLON n_BLOCK T_DOT");
                        tprint("T_IDENT", $T_IDENT.text);
                        tprint("T_SCOLON", $T_SCOLON.text);
                        tprint("T_DOT", $T_DOT.text);
                    }
                ;
n_PROGLBL       : T_PROG
                    {
                    	prRule("n_PROGLBL", "T_PROG");
                        tprint("T_PROG", $T_PROG.text);
                    }
                ;
n_READ          : T_READ T_LPAREN n_INPUTVAR n_INPUTLST T_RPAREN
                    {
                    	prRule("n_READ","T_READ T_LPAREN n_INPUTVAR n_INPUTLST T_RPAREN");
                        tprint("T_READ", $T_READ.text);
                        tprint("T_LPAREN", $T_LPAREN.text);
                    }
                ;
n_RELOP         : T_LT
                    {
                    	prRule("n_RELOP", "T_LT");
                        tprint("T_LT", $T_LT.text);
                    }
                | T_GT
                    {
                    	prRule("n_RELOP", "T_GT");
                        tprint("T_GT", $T_GT.text);
                    }
                | T_LE
                    {
                    	prRule("n_RELOP", "T_LE");
                        tprint("T_LE", $T_LE.text);
                    }
                | T_GE
                    {
                    	prRule("n_RELOP", "T_GE");
                        tprint("T_GE", $T_GE.text);
                    }
                | T_EQ
                    {
                    	prRule("n_RELOP", "T_EQ");
                        tprint("T_EQ", $T_EQ.text);
                    }
                | T_NE
                    {
                    	prRule("n_RELOP", "T_NE");
                        tprint("T_NE", $T_NE.text);
                    }
                ;
n_SIGN          : /* epsilon */
                    {
                    	prRule("n_SIGN", "epsilon");
                    }
                | T_PLUS
                    {
                    	prRule("n_SIGN", "T_PLUS");
                        tprint("T_PLUS", $T_PLUS.text);
                    }
                | T_MINUS
                    {
                    	prRule("n_SIGN", "T_MINUS");
                        tprint("T_MINUS", $T_MINUS.text);
                    }
                ;
n_SIMPLE        : T_INT
                    {
                    	prRule("n_SIMPLE", "T_INT");
                        tprint("T_INT", $T_INT.text);
                    }
                | T_CHAR
                    {
                    	prRule("n_SIMPLE", "T_CHAR");
                        tprint("T_CHAR", $T_CHAR.text);
                    }
                | T_BOOL
                    {
                    	prRule("n_SIMPLE", "T_BOOL");
                        tprint("T_BOOL", $T_BOOL.text);
                    }
                ;
n_SIMPLEEXPR    : n_TERM n_ADDOPLST
                    {
                        	prRule("n_SIMPLEEXPR", "n_TERM n_ADDOPLST");
                    }
                ;
n_STMT          : n_ASSIGN
                    {
                        	prRule("n_STMT", "n_ASSIGN");
                    }
                | n_PROCSTMT
                    {
                        	prRule("n_STMT", "n_PROCSTMT");
                    }
                | n_READ
                    {
                        	prRule("n_STMT", "n_READ");
                    }
                | n_WRITE
                    {
                        	prRule("n_STMT", "n_WRITE");
                    }
                | n_CONDITION
                    {
                        	prRule("n_STMT", "n_CONDITION");
                    }
                | n_WHILE
                    {
                        	prRule("n_STMT", "n_WHILE");
                    }
                | n_COMPOUND
                    {
                        	prRule("n_STMT", "n_COMPOUND");
                    }
                ;
n_STMTLST       : /* epsilon */
                    {
                        	prRule("n_STMTLST", "epsilon");
                    }
                | T_SCOLON n_STMT n_STMTLST
                    {
                        	prRule("n_STMTLST", "T_SCOLON n_STMT n_STMTLST");
                            tprint("T_SCOLON", $T_SCOLON.text);
                    }
                ;
n_STMTPART      : n_COMPOUND
                    {
                        	prRule("n_STMTPART", "n_COMPOUND");
                    }
                ;
n_TERM          : n_FACTOR n_MULTOPLST
                    {
                        	prRule("n_TERM", "n_FACTOR n_MULTOPLST");
                    }
                ;
n_TYPE          : n_SIMPLE
                    {
                        	prRule("n_TYPE", "n_SIMPLE");
                    }
                | n_ARRAY
                    {
                        	prRule("n_TYPE", "n_ARRAY");
                    }
                ;
n_VARDEC        : n_IDENT n_IDENTLST T_COLON n_TYPE
                    {
                        	prRule("n_VARDEC", "n_IDENT n_IDENTLST T_COLON n_TYPE");
                            tprint("T_COLON", $T_COLON.text);
                    }
                ;
n_VARDECLST     : /* epsilon */
                    {
                        	prRule("n_VARDECLST", "epsilon");
                    }
                | n_VARDEC T_SCOLON n_VARDECLST
                    {
                        	prRule("n_VARDECLST", "n_VARDEC T_SCOLON n_VARDECLST");
                            tprint("T_SCOLON", $T_SCOLON.text);
                    }
                ;
n_VARDECPART    : /* epsilon */
                    {
                        	prRule("n_VARDECPART", "epsilon");
                    }
                | T_VAR n_VARDEC T_SCOLON n_VARDECLST
                    {
                        	prRule("n_VARDECPART", "T_VAR n_VARDEC T_SCOLON n_VARDECLST");
                            tprint("T_VAR", $T_VAR.text);
                            tprint("T_SCOLON", $T_SCOLON.text);
                    }
                ;
n_VARIABLE      : n_ENTIREVAR
                    {
                        	prRule("n_VARIABLE", "n_ENTIREVAR");
                    }
                | n_IDXVAR
                    {
                        	prRule("n_VARIABLE", "n_IDXVAR");
                    }
                ;
n_VARIDENT      : T_IDENT
                    {
                        	prRule("n_VARIDENT", "T_IDENT");
                            tprint("T_IDENT", $T_IDENT.text);
                    }
                ;
n_WHILE         : T_WHILE n_EXPR T_DO n_STMT
                    {
                        	prRule("n_WHILE", "T_WHILE n_EXPR T_DO n_STMT");
                            tprint("T_WHILE", $T_WHILE.text);
                            tprint("T_DO", $T_DO.text);
                    }
                ;
n_WRITE         : T_WRITE T_LPAREN n_OUTPUT n_OUTPUTLST T_RPAREN
                    {
                        	prRule("n_WRITE", "T_WRITE T_LPAREN n_OUTPUT n_OUTPUTLST T_RPAREN");
                            tprint("T_WRITE", $T_WRITE.text);
                            tprint("T_LPAREN", $T_LPAREN.text);
                            tprint("T_RPAREN", $T_RPAREN.text);
                    }
                ;




/*  LEXER */
T_COMMENT: '(*' .*? '*)' -> skip ;
T_LPAREN: '('       ;  
T_RPAREN: ')'       ;      
T_MULT: '*'         ;     
T_PLUS: '+'         ;    
T_COMMA: ','        ;   
T_MINUS: '-'        ;  
T_DOTDOT: '..'      ; 
T_DOT: '.'          ;
T_COLON: ':'        ;
T_ASSIGN: ':='      ;
T_SCOLON: ';'       ;
T_LT: '<'           ;
T_LE: '<='          ;
T_NE: '<>'          ;
T_EQ: '='           ;
T_GT: '>'           ;
T_GE: '>='          ;
T_LBRACK: '['       ;
T_RBRACK: ']'       ;
T_AND: 'and'        ;
T_ARRAY: 'array'    ;
T_BEGIN: 'begin'    ;
T_BOOL: 'boolean'   ;
T_CHAR: 'char'      ;
T_DIV: 'div'        ;
T_DO: 'do'          ;
T_ELSE: 'else'      ;
T_END: 'end'        ;
T_FALSE: 'false'    ;
T_IF: 'if'          ;
T_INT: 'integer'    ;
T_NOT: 'not'        ;
T_OF: 'of'          ;
T_OR: 'or'          ;
T_PROC: 'procedure' ;
T_PROG: 'program'   ;
T_READ: 'read'      ;
T_THEN: 'then'      ;
T_TRUE: 'true'      ;
T_VAR: 'var'        ;
T_WHILE: 'while'    ;
T_WRITE: 'write'    ;

T_INTCONST:   ZERO* COUNT_DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT 
            | ZERO* COUNT_DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT 
            | ZERO* COUNT_DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT 
            | ZERO* COUNT_DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT 
            | ZERO* COUNT_DIGIT DIGIT DIGIT DIGIT DIGIT 
            | ZERO* COUNT_DIGIT DIGIT DIGIT DIGIT 
            | ZERO* COUNT_DIGIT DIGIT DIGIT 
            | ZERO* COUNT_DIGIT DIGIT 
            | ZERO* COUNT_DIGIT
            | ZERO+ ;

T_MAYBEINT:  ZERO* COUNT_DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT ;

T_INVALINT: ZERO* COUNT_DIGIT DIGIT* {setType(T_UNKNOWN);} ;

T_IDENT: (UNDERSCORE|ALPHA)(ALPHANUM|UNDERSCORE)* ;

T_BADCHARCONST: QUOTE|QUOTE QUOTE {setType(T_UNKNOWN);} ;

T_CHARCONST: QUOTE.QUOTE ;

WS: [ \t\r\n]+ -> skip ;

T_UNKNOWN: . ;

fragment
ZERO: '0' ;

fragment
COUNT_DIGIT: [1-9] ;

fragment
DIGIT: ZERO|[1-9] ;

fragment
QUOTE: '\'' ;

fragment
UNDERSCORE: '_' ;

fragment
SIGN: [+-] ;

fragment
LO_ALPHA: [a-z] ;

fragment
HI_ALPHA: [A-Z] ;

fragment
ALPHA: LO_ALPHA | HI_ALPHA ;

fragment
ALPHANUM: ALPHA | DIGIT ;

fragment
CHARCONST: QUOTE.QUOTE ;





