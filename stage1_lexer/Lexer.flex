// section 1: user code copied verbatim

%%

// section 2: options and declarations

%class Lexer
%unicode
// Not enabling yyline, because the definition of LineTerminator for COOL is different from the JFlex default
// %line
%column
%type Token
// The following three states each marks a special processing block for a type of Token
%state COMMENT
%state STRING
%state INLINECOMMENT

%{
    // Code here is copied verbatim into the Lexer class

    // This is a State representing the level of nested comments
    public int comment_level = 0;

    // This variable tracks the current line number
    public int line_num = 1;

    // This variable is used to efficiently store and concat string segments
    StringBuffer string = new StringBuffer();

    // This variable stores the starting line number of the currently reading string.
    // So that special characters do not affect Token's line number
    int string_start_line = 0;

    /* The following two methods instantiate Token objects */
    private Token token(tok type) {
        return new Token(type, line_num , yycolumn + 1, null);
    }

    /* when there is a lexeme for the Token */
    private Token token(tok type, Object lexeme) {
        return new Token(type, line_num, yycolumn + 1, lexeme);
    }

    /* An abstract method that increment line_num
     * to avoid numeric mistakes.
     */
    private void incrementLine()
    {
        line_num += 1;
    }

%}

// define some macros
Identifier = [a-z][_A-Za-z0-9]*
IntegerLiteral = [0-9]+
Type = [A-Z][_A-Za-z0-9]*
InlineComment = --[. \r\013\f\t]*\n

/* The following block of macros deal with keywords case-insensitivity */
Case = [cC][aA][sS][eE]
Class = [cC][lL][aA][sS][sS]
Else = [eE][lL][sS][eE]
Esac = [eE][sS][aA][cC]
Fi = [fF][iI]
If = [iI][fF]
In = [iI][nN]
Inherits = [iI][nN][hH][eE][rR][iI][tT][sS]
Isvoid = [iI][sS][vV][oO][iI][dD]
Let = [lL][eE][tT]
Loop = [lL][oO][oO][pP]
New = [nN][eE][wW]
Not = [nN][oO][tT]
Of = [oO][fF]
Pool = [pP][oO][oO][lL]
Then = [tT][hH][eE][nN]
While = [wW][hH][iI][lL][eE]

/* True False deserves special treatment */
True = t[rR][uU][eE]
False = f[aA][lL][sS][eE]

/* WhiteSpaces, excluding \n and \r */
WhiteSpaces = [ \r\013\f\t]+


%%

// section 3: Lexical rules


// default table
<YYINITIAL> {
    "--"       { yybegin(INLINECOMMENT); }                  // entering INLINECOMMENT block
    "(*"       { yybegin(COMMENT); comment_level = 1; }     // start COMMENT block
    \"         { string.setLength(0); yybegin(STRING); }    // start STRING block
    "\n"       { line_num += 1; }                           // \n does not require processing, but line_num increments

    /* The following are literal matches for simple char keywords/operators */
    "@"        { return token(tok.AT);       }
    ":"        { return token(tok.COLON);    }
    ","        { return token(tok.COMMA);    }
    "/"        { return token(tok.DIVIDE);   }
    "."        { return token(tok.DOT);      }
    "="        { return token(tok.EQUALS);   }
    "<-"       { return token(tok.LARROW);   }
    "{"        { return token(tok.LBRACE);   }
    "<="       { return token(tok.LE);       }
    "("        { return token(tok.LPAREN);   }
    "<"        { return token(tok.LT);       }
    "-"        { return token(tok.MINUS);    }
    "+"        { return token(tok.PLUS);     }
    "=>"       { return token(tok.RARROW);   }
    "}"        { return token(tok.RBRACE);   }
    ")"        { return token(tok.RPAREN);   }
    ";"        { return token(tok.SEMI);     }
    "~"        { return token(tok.TILDE);    }
    "*"        { return token(tok.TIMES);    }
    {Case}           { return token(tok.CASE);      }
    {Class}          { return token(tok.CLASS);     }
    {Else}           { return token(tok.ELSE);      }
    {Esac}           { return token(tok.ESAC);      }
    {Fi}             { return token(tok.FI);        }
    {If}             { return token(tok.IF);        }
    {In}             { return token(tok.IN);        }
    {Inherits}       { return token(tok.INHERITS);  }
    {Isvoid}         { return token(tok.ISVOID);    }
    {Let}            { return token(tok.LET);       }
    {Loop}           { return token(tok.LOOP);      }
    {New}            { return token(tok.NEW);       }
    {Not}            { return token(tok.NOT);       }
    {Of}             { return token(tok.OF);        }
    {Pool}           { return token(tok.POOL);      }
    {Then}           { return token(tok.THEN);      }
    {While}          { return token(tok.WHILE);     }
    {True}           { return token(tok.TRUE);      }
    {False}          { return token(tok.FALSE);     }
    {Identifier}     { return token(tok.IDENTIFIER, yytext());}
    {IntegerLiteral} { return token(tok.INTEGER,    yytext());}
    {Type}           { return token(tok.TYPE,       yytext());}
    {WhiteSpaces}    { /* ignore */ }
    // catch-all regex that prompt an error because the character is unrecognized
    [^]              { throw new Error("ERROR: " + line_num + ": Lexer: invalid character: " + yytext()); }
}

/* INLINECOMMENT Processing */
<INLINECOMMENT> {
    "\n"        { yypushback(1); yybegin(YYINITIAL); }
    [^]         { }
}

/* COMMENT Processing */
<COMMENT> {
    "(*"        { comment_level += 1; } // entering another level of nested comment

    "*)"        {
                    comment_level--;  // leaving a level of nested comment
                    if (comment_level == 0) { // if already outside, go back to default processing
                        yybegin(YYINITIAL);
                    }
                }
    "\n"        { incrementLine(); }
    <<EOF>>     { throw new Error("ERROR: " + line_num + ": Lexer: EOF in comment ");}
    [^]         { }

}

/* STRING processing
 * Because in string, an unescaped newline is not allowed, there will not be a case where line number increases inside processing of a string literal. Therefore, no rules below increments line number.
 */

<STRING> {
    \"                      {   // always marks the end of a string
                                yybegin(YYINITIAL);
                                return token(tok.STRING, string.toString());
                            }

    // NULL is not allowed
    \0                      { throw new Error("ERROR: " + line_num + ": Lexer: NUL in String"); }
    // EOF is not allowed
    <<EOF>>                 { throw new Error("ERROR: " + line_num + ": Lexer: Unclosed String"); }
    // \r is treated as a normal character
    \013                    { string.append(yytext()); }
    // escaped chars are allowed, excluding \n, \0, <<EOF>>
    (\\[^\n\0<<EOF>>])+     { string.append(yytext()); }
    // unescaped chars are allowed, excluding \n, \0, backslash
    [^\\\"\n\0]+            { string.append(yytext()); }
    // Catch-all case (\n is included in this case, because the error message is identical to invalid character
    [^]                     { throw new Error("ERROR: " + line_num + ": Lexer: Invalid character \""); }
}





