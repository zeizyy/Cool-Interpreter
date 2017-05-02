import java.util.HashSet;

/* Token Class: The data structure that stores:
 * 1) token type
 * 2) line number and column number
 * 3) lexeme (optional)
 */
public class Token {
    // the lexeme
    private Object lexeme;
    // the token type
    private tok tokType;
    // the line number
    private int line, col;

    // A method that determines whether an int literal is outside the boundary
    private boolean int_exceed_max(String lexeme)
    {
        String maxNum = "2147483647";
        if (lexeme.length() > maxNum.length())
            return true;
        else if (lexeme.length() == maxNum.length())
            return lexeme.compareTo(maxNum) > 0;
        else
            return false;
    }

    // Constructor of Token objects. Does a few error checkings.
    public Token(tok type, int line, int col, Object value) {
        this.tokType = type;
        this.line = line;
        this.col = col;
        this.lexeme = value;
        if (type == tok.INTEGER)
        {   // Clean the integer literal and check whether it is within boundary
            String literal = (String) value;
            literal = literal.replaceFirst("^0+(?!$)", "");

            if (int_exceed_max(literal)) {
                lexer_err("not a non-negative 32-bit signed integer", line, literal);
            } else {
                this.lexeme = literal;
            }
        }
        else if (type == tok.STRING)
        {   // Error checking for STRING token
            String val = (String)value;
            if (val.length() > 1024)
                lexer_err("string constant is too long (" + val.length() + " > " + "1024)", line, "");
        }
    }

    // A wrapper message of error printing
    private void lexer_err(String message, int line_num, String content) {
            throw new Error("ERROR: " + line_num + ": Lexer: " + message + ": " + content);
    }

    // ToString method of Token for easier printing
    public String toString() {
        String retVal =  this.line + "\n" + this.tokType.name;
        if (this.lexeme == null) {
            return retVal;
        } else {
            return retVal + '\n' + this.lexeme;
        }
    }
}

