/* The following code was generated by JFlex 1.6.1 */

// section 1: user code copied verbatim


/**
 * This class is a scanner generated by 
 * <a href="http://www.jflex.de/">JFlex</a> 1.6.1
 * from the specification file <tt>stage1_lexer/Lexer.flex</tt>
 */
class Lexer {

  /** This character denotes the end of file */
  public static final int YYEOF = -1;

  /** initial size of the lookahead buffer */
  private static final int ZZ_BUFFERSIZE = 16384;

  /** lexical states */
  public static final int YYINITIAL = 0;
  public static final int COMMENT = 2;
  public static final int STRING = 4;
  public static final int INLINECOMMENT = 6;

  /**
   * ZZ_LEXSTATE[l] is the state in the DFA for the lexical state l
   * ZZ_LEXSTATE[l+1] is the state in the DFA for the lexical state l
   *                  at the beginning of a line
   * l is of the form l = 2*k, k a non negative integer
   */
  private static final int ZZ_LEXSTATE[] = { 
     0,  0,  1,  1,  2,  2,  3, 3
  };

  /** 
   * Translates characters to character classes
   */
  private static final String ZZ_CMAP_PACKED = 
    "\1\73\10\0\1\52\1\7\1\52\2\52\22\0\1\52\1\0\1\55"+
    "\5\0\1\53\1\70\1\54\1\65\1\60\1\5\1\6\1\61\12\3"+
    "\1\57\1\71\1\63\1\62\1\66\1\0\1\56\1\13\1\4\1\11"+
    "\1\43\1\17\1\23\1\4\1\31\1\25\2\4\1\21\1\4\1\27"+
    "\1\41\1\45\1\4\1\33\1\15\1\35\1\51\1\37\1\47\3\4"+
    "\1\0\1\74\2\0\1\2\1\0\1\12\1\1\1\10\1\42\1\16"+
    "\1\22\1\1\1\30\1\24\2\1\1\20\1\1\1\26\1\40\1\44"+
    "\1\1\1\32\1\14\1\34\1\50\1\36\1\46\3\1\1\64\1\0"+
    "\1\67\1\72\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uff91\0";

  /** 
   * Translates characters to character classes
   */
  private static final char [] ZZ_CMAP = zzUnpackCMap(ZZ_CMAP_PACKED);

  /** 
   * Translates DFA states to action switch labels.
   */
  private static final int [] ZZ_ACTION = zzUnpackAction();

  private static final String ZZ_ACTION_PACKED_0 =
    "\4\0\1\1\1\2\1\3\1\4\1\5\1\6\1\7"+
    "\1\2\1\4\1\2\1\4\1\2\1\4\1\2\1\4"+
    "\1\2\1\4\1\2\1\4\1\2\1\4\1\2\1\4"+
    "\1\2\1\4\1\2\1\4\1\10\1\11\1\12\1\13"+
    "\1\14\1\15\1\16\1\17\1\20\1\21\1\22\1\23"+
    "\1\24\1\25\1\26\1\27\1\30\1\31\2\30\1\32"+
    "\1\33\1\34\1\35\1\33\1\36\1\37\2\2\2\4"+
    "\2\2\2\4\2\2\2\4\1\2\2\40\1\2\1\41"+
    "\1\42\1\4\1\41\1\42\2\2\2\4\2\2\1\4"+
    "\2\43\1\2\1\4\1\2\1\4\1\44\1\45\1\46"+
    "\1\47\1\50\1\51\1\32\2\2\2\4\2\2\2\4"+
    "\1\52\1\2\1\52\1\4\3\2\2\4\1\53\1\54"+
    "\1\53\1\54\2\2\1\4\1\2\1\4\1\2\1\4"+
    "\1\0\1\55\1\2\1\55\1\4\1\56\1\57\1\56"+
    "\1\57\2\60\3\2\2\4\1\61\1\62\1\61\2\63"+
    "\1\2\1\4\2\64\1\65\2\2\2\4\2\66\1\67"+
    "\1\2\1\67\1\4\1\2\1\4\2\70";

  private static int [] zzUnpackAction() {
    int [] result = new int[167];
    int offset = 0;
    offset = zzUnpackAction(ZZ_ACTION_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAction(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /** 
   * Translates a state to a row index in the transition table
   */
  private static final int [] ZZ_ROWMAP = zzUnpackRowMap();

  private static final String ZZ_ROWMAP_PACKED_0 =
    "\0\0\0\75\0\172\0\267\0\364\0\u0131\0\u016e\0\u01ab"+
    "\0\u01e8\0\364\0\364\0\u0225\0\u0262\0\u029f\0\u02dc\0\u0319"+
    "\0\u0356\0\u0393\0\u03d0\0\u040d\0\u044a\0\u0487\0\u04c4\0\u0501"+
    "\0\u053e\0\u057b\0\u05b8\0\u05f5\0\u0632\0\u066f\0\u06ac\0\u06e9"+
    "\0\u0726\0\364\0\364\0\364\0\364\0\364\0\364\0\u0763"+
    "\0\u07a0\0\364\0\364\0\364\0\364\0\364\0\364\0\364"+
    "\0\364\0\u07dd\0\u081a\0\u0857\0\364\0\364\0\364\0\u0894"+
    "\0\364\0\364\0\u08d1\0\u090e\0\u094b\0\u0988\0\u09c5\0\u0a02"+
    "\0\u0a3f\0\u0a7c\0\u0ab9\0\u0af6\0\u0b33\0\u0b70\0\u0bad\0\u0131"+
    "\0\u01ab\0\u0bea\0\u0131\0\u0c27\0\u0c64\0\u01ab\0\u0ca1\0\u0cde"+
    "\0\u0d1b\0\u0d58\0\u0d95\0\u0dd2\0\u0e0f\0\u0e4c\0\u0131\0\u01ab"+
    "\0\u0e89\0\u0ec6\0\u0f03\0\u0f40\0\364\0\364\0\364\0\364"+
    "\0\364\0\364\0\u0f7d\0\u0fba\0\u0ff7\0\u1034\0\u1071\0\u10ae"+
    "\0\u10eb\0\u1128\0\u1165\0\u0131\0\u11a2\0\u01ab\0\u11df\0\u121c"+
    "\0\u1259\0\u1296\0\u12d3\0\u1310\0\u0131\0\u0131\0\u01ab\0\u01ab"+
    "\0\u134d\0\u138a\0\u13c7\0\u1404\0\u1441\0\u147e\0\u14bb\0\u0894"+
    "\0\u0131\0\u14f8\0\u01ab\0\u1535\0\u0131\0\u0131\0\u01ab\0\u01ab"+
    "\0\u0131\0\u01ab\0\u1572\0\u15af\0\u15ec\0\u1629\0\u1666\0\u0131"+
    "\0\u0131\0\u01ab\0\u0131\0\u01ab\0\u16a3\0\u16e0\0\u0131\0\u01ab"+
    "\0\u0131\0\u171d\0\u175a\0\u1797\0\u17d4\0\u0131\0\u01ab\0\u0131"+
    "\0\u1811\0\u01ab\0\u184e\0\u188b\0\u18c8\0\u0131\0\u01ab";

  private static int [] zzUnpackRowMap() {
    int [] result = new int[167];
    int offset = 0;
    offset = zzUnpackRowMap(ZZ_ROWMAP_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackRowMap(String packed, int offset, int [] result) {
    int i = 0;  /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int high = packed.charAt(i++) << 16;
      result[j++] = high | packed.charAt(i++);
    }
    return j;
  }

  /** 
   * The transition table of the DFA
   */
  private static final int [] ZZ_TRANS = zzUnpackTrans();

  private static final String ZZ_TRANS_PACKED_0 =
    "\1\5\1\6\1\5\1\7\1\10\1\11\1\12\1\13"+
    "\1\14\1\15\1\6\1\10\1\6\1\10\1\16\1\17"+
    "\1\20\1\21\1\22\1\23\1\24\1\25\1\26\1\27"+
    "\1\6\1\10\1\6\1\10\1\30\1\31\1\6\1\10"+
    "\1\32\1\33\1\6\1\10\1\34\1\35\1\36\1\37"+
    "\1\6\1\10\1\40\1\41\1\42\1\43\1\44\1\45"+
    "\1\46\1\47\1\50\1\51\1\52\1\53\1\5\1\54"+
    "\1\55\1\56\1\57\2\5\7\60\1\61\43\60\1\62"+
    "\1\63\20\60\7\64\1\65\45\64\1\66\15\64\1\67"+
    "\1\70\7\60\1\71\65\60\76\0\4\6\3\0\42\6"+
    "\26\0\1\7\72\0\4\10\3\0\42\10\30\0\1\72"+
    "\70\0\4\6\3\0\2\6\2\73\4\6\2\74\30\6"+
    "\24\0\4\10\3\0\2\10\2\75\4\10\2\76\30\10"+
    "\24\0\4\6\3\0\4\6\2\77\2\6\2\100\30\6"+
    "\24\0\4\10\3\0\4\10\2\101\2\10\2\102\30\10"+
    "\24\0\4\6\3\0\6\6\2\103\20\6\2\104\10\6"+
    "\24\0\4\10\3\0\6\10\2\105\20\10\2\106\10\10"+
    "\24\0\4\6\3\0\2\6\2\107\10\6\2\110\24\6"+
    "\24\0\4\10\3\0\14\10\2\111\24\10\24\0\4\6"+
    "\3\0\4\6\2\112\4\6\2\113\2\6\2\114\22\6"+
    "\24\0\4\10\3\0\4\10\2\115\4\10\2\116\2\10"+
    "\2\117\22\10\24\0\4\6\3\0\6\6\2\120\20\6"+
    "\2\121\10\6\24\0\4\10\3\0\6\10\2\122\20\10"+
    "\2\123\10\10\24\0\4\6\3\0\20\6\2\124\2\125"+
    "\16\6\24\0\4\10\3\0\20\10\2\126\20\10\24\0"+
    "\4\6\3\0\12\6\2\127\26\6\24\0\4\10\3\0"+
    "\12\10\2\130\26\10\24\0\4\6\3\0\30\6\2\131"+
    "\10\6\24\0\4\10\3\0\30\10\2\132\10\10\24\0"+
    "\4\6\3\0\20\6\2\133\20\6\24\0\4\10\3\0"+
    "\20\10\2\134\20\10\75\0\1\40\76\0\1\135\106\0"+
    "\1\136\13\0\1\137\54\0\1\140\66\0\1\141\110\0"+
    "\1\142\4\0\7\64\1\0\45\64\1\0\15\64\2\0"+
    "\7\143\1\0\7\143\1\0\3\143\1\0\15\143\1\0"+
    "\21\143\1\0\2\143\1\0\4\143\1\0\1\143\1\0"+
    "\4\6\3\0\4\6\2\144\34\6\24\0\4\6\3\0"+
    "\2\6\2\145\36\6\24\0\4\10\3\0\4\10\2\146"+
    "\34\10\24\0\4\10\3\0\2\10\2\147\36\10\24\0"+
    "\4\6\3\0\2\6\2\150\36\6\24\0\4\6\3\0"+
    "\4\6\2\151\34\6\24\0\4\10\3\0\2\10\2\152"+
    "\36\10\24\0\4\10\3\0\4\10\2\153\34\10\24\0"+
    "\4\6\3\0\24\6\2\154\14\6\24\0\4\6\3\0"+
    "\30\6\2\155\10\6\24\0\4\10\3\0\24\10\2\156"+
    "\14\10\24\0\4\10\3\0\30\10\2\157\10\10\24\0"+
    "\4\6\3\0\10\6\2\160\30\6\24\0\4\6\3\0"+
    "\26\6\2\161\12\6\24\0\4\6\3\0\20\6\2\162"+
    "\20\6\24\0\4\10\3\0\26\10\2\163\12\10\24\0"+
    "\4\10\3\0\20\10\2\164\20\10\24\0\4\6\3\0"+
    "\36\6\2\165\2\6\24\0\4\6\3\0\24\6\2\166"+
    "\14\6\24\0\4\10\3\0\36\10\2\167\2\10\24\0"+
    "\4\10\3\0\24\10\2\170\14\10\24\0\4\6\3\0"+
    "\6\6\2\171\32\6\24\0\4\6\3\0\40\6\2\172"+
    "\24\0\4\10\3\0\6\10\2\173\32\10\24\0\4\6"+
    "\3\0\30\6\2\174\10\6\24\0\4\10\3\0\30\10"+
    "\2\175\10\10\24\0\4\6\3\0\14\6\2\176\24\6"+
    "\24\0\4\10\3\0\14\10\2\177\24\10\117\0\1\200"+
    "\1\0\4\6\3\0\6\6\2\201\32\6\24\0\4\6"+
    "\3\0\4\6\2\202\34\6\24\0\4\10\3\0\6\10"+
    "\2\203\32\10\24\0\4\10\3\0\4\10\2\204\34\10"+
    "\24\0\4\6\3\0\2\205\40\6\24\0\4\6\3\0"+
    "\6\6\2\206\32\6\24\0\4\10\3\0\2\207\40\10"+
    "\24\0\4\10\3\0\6\10\2\210\32\10\24\0\4\6"+
    "\3\0\34\6\2\211\4\6\24\0\4\10\3\0\34\10"+
    "\2\212\4\10\24\0\4\6\3\0\4\6\2\213\34\6"+
    "\24\0\4\6\3\0\30\6\2\214\10\6\24\0\4\6"+
    "\3\0\6\6\2\215\32\6\24\0\4\10\3\0\30\10"+
    "\2\216\10\10\24\0\4\10\3\0\6\10\2\217\32\10"+
    "\24\0\4\6\3\0\16\6\2\220\22\6\24\0\4\6"+
    "\3\0\6\6\2\221\32\6\24\0\4\10\3\0\16\10"+
    "\2\222\22\10\24\0\4\6\3\0\10\6\2\223\30\6"+
    "\24\0\4\10\3\0\10\10\2\224\30\10\24\0\4\6"+
    "\3\0\10\6\2\225\30\6\24\0\4\10\3\0\10\10"+
    "\2\226\30\10\24\0\4\6\3\0\4\6\2\227\34\6"+
    "\24\0\4\10\3\0\4\10\2\230\34\10\24\0\4\6"+
    "\3\0\6\6\2\231\32\6\24\0\4\6\3\0\14\6"+
    "\2\232\24\6\24\0\4\6\3\0\22\6\2\233\16\6"+
    "\24\0\4\10\3\0\14\10\2\234\24\10\24\0\4\10"+
    "\3\0\22\10\2\235\16\10\24\0\4\6\3\0\6\6"+
    "\2\236\32\6\24\0\4\10\3\0\6\10\2\237\32\10"+
    "\24\0\4\6\3\0\32\6\2\240\6\6\24\0\4\6"+
    "\3\0\14\6\2\241\24\6\24\0\4\10\3\0\32\10"+
    "\2\242\6\10\24\0\4\10\3\0\14\10\2\243\24\10"+
    "\24\0\4\6\3\0\24\6\2\244\14\6\24\0\4\10"+
    "\3\0\24\10\2\245\14\10\24\0\4\6\3\0\4\6"+
    "\2\246\34\6\24\0\4\10\3\0\4\10\2\247\34\10"+
    "\23\0";

  private static int [] zzUnpackTrans() {
    int [] result = new int[6405];
    int offset = 0;
    offset = zzUnpackTrans(ZZ_TRANS_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackTrans(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      value--;
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /* error codes */
  private static final int ZZ_UNKNOWN_ERROR = 0;
  private static final int ZZ_NO_MATCH = 1;
  private static final int ZZ_PUSHBACK_2BIG = 2;

  /* error messages for the codes above */
  private static final String ZZ_ERROR_MSG[] = {
    "Unknown internal scanner error",
    "Error: could not match input",
    "Error: pushback value was too large"
  };

  /**
   * ZZ_ATTRIBUTE[aState] contains the attributes of state <code>aState</code>
   */
  private static final int [] ZZ_ATTRIBUTE = zzUnpackAttribute();

  private static final String ZZ_ATTRIBUTE_PACKED_0 =
    "\4\0\1\11\4\1\2\11\26\1\6\11\2\1\10\11"+
    "\3\1\3\11\1\1\2\11\42\1\6\11\35\1\1\0"+
    "\47\1";

  private static int [] zzUnpackAttribute() {
    int [] result = new int[167];
    int offset = 0;
    offset = zzUnpackAttribute(ZZ_ATTRIBUTE_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAttribute(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }

  /** the input device */
  private java.io.Reader zzReader;

  /** the current state of the DFA */
  private int zzState;

  /** the current lexical state */
  private int zzLexicalState = YYINITIAL;

  /** this buffer contains the current text to be matched and is
      the source of the yytext() string */
  private char zzBuffer[] = new char[ZZ_BUFFERSIZE];

  /** the textposition at the last accepting state */
  private int zzMarkedPos;

  /** the current text position in the buffer */
  private int zzCurrentPos;

  /** startRead marks the beginning of the yytext() string in the buffer */
  private int zzStartRead;

  /** endRead marks the last character in the buffer, that has been read
      from input */
  private int zzEndRead;

  /** number of newlines encountered up to the start of the matched text */
  private int yyline;

  /** the number of characters up to the start of the matched text */
  private int yychar;

  /**
   * the number of characters from the last newline up to the start of the 
   * matched text
   */
  private int yycolumn;

  /** 
   * zzAtBOL == true <=> the scanner is currently at the beginning of a line
   */
  private boolean zzAtBOL = true;

  /** zzAtEOF == true <=> the scanner is at the EOF */
  private boolean zzAtEOF;

  /** denotes if the user-EOF-code has already been executed */
  private boolean zzEOFDone;
  
  /** 
   * The number of occupied positions in zzBuffer beyond zzEndRead.
   * When a lead/high surrogate has been read from the input stream
   * into the final zzBuffer position, this will have a value of 1;
   * otherwise, it will have a value of 0.
   */
  private int zzFinalHighSurrogate = 0;

  /* user code: */
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



  /**
   * Creates a new scanner
   *
   * @param   in  the java.io.Reader to read input from.
   */
  Lexer(java.io.Reader in) {
    this.zzReader = in;
  }


  /** 
   * Unpacks the compressed character translation table.
   *
   * @param packed   the packed character translation table
   * @return         the unpacked character translation table
   */
  private static char [] zzUnpackCMap(String packed) {
    char [] map = new char[0x110000];
    int i = 0;  /* index in packed string  */
    int j = 0;  /* index in unpacked array */
    while (i < 198) {
      int  count = packed.charAt(i++);
      char value = packed.charAt(i++);
      do map[j++] = value; while (--count > 0);
    }
    return map;
  }


  /**
   * Refills the input buffer.
   *
   * @return      <code>false</code>, iff there was new input.
   * 
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  private boolean zzRefill() throws java.io.IOException {

    /* first: make room (if you can) */
    if (zzStartRead > 0) {
      zzEndRead += zzFinalHighSurrogate;
      zzFinalHighSurrogate = 0;
      System.arraycopy(zzBuffer, zzStartRead,
                       zzBuffer, 0,
                       zzEndRead-zzStartRead);

      /* translate stored positions */
      zzEndRead-= zzStartRead;
      zzCurrentPos-= zzStartRead;
      zzMarkedPos-= zzStartRead;
      zzStartRead = 0;
    }

    /* is the buffer big enough? */
    if (zzCurrentPos >= zzBuffer.length - zzFinalHighSurrogate) {
      /* if not: blow it up */
      char newBuffer[] = new char[zzBuffer.length*2];
      System.arraycopy(zzBuffer, 0, newBuffer, 0, zzBuffer.length);
      zzBuffer = newBuffer;
      zzEndRead += zzFinalHighSurrogate;
      zzFinalHighSurrogate = 0;
    }

    /* fill the buffer with new input */
    int requested = zzBuffer.length - zzEndRead;
    int numRead = zzReader.read(zzBuffer, zzEndRead, requested);

    /* not supposed to occur according to specification of java.io.Reader */
    if (numRead == 0) {
      throw new java.io.IOException("Reader returned 0 characters. See JFlex examples for workaround.");
    }
    if (numRead > 0) {
      zzEndRead += numRead;
      /* If numRead == requested, we might have requested to few chars to
         encode a full Unicode character. We assume that a Reader would
         otherwise never return half characters. */
      if (numRead == requested) {
        if (Character.isHighSurrogate(zzBuffer[zzEndRead - 1])) {
          --zzEndRead;
          zzFinalHighSurrogate = 1;
        }
      }
      /* potentially more input available */
      return false;
    }

    /* numRead < 0 ==> end of stream */
    return true;
  }

    
  /**
   * Closes the input stream.
   */
  public final void yyclose() throws java.io.IOException {
    zzAtEOF = true;            /* indicate end of file */
    zzEndRead = zzStartRead;  /* invalidate buffer    */

    if (zzReader != null)
      zzReader.close();
  }


  /**
   * Resets the scanner to read from a new input stream.
   * Does not close the old reader.
   *
   * All internal variables are reset, the old input stream 
   * <b>cannot</b> be reused (internal buffer is discarded and lost).
   * Lexical state is set to <tt>ZZ_INITIAL</tt>.
   *
   * Internal scan buffer is resized down to its initial length, if it has grown.
   *
   * @param reader   the new input stream 
   */
  public final void yyreset(java.io.Reader reader) {
    zzReader = reader;
    zzAtBOL  = true;
    zzAtEOF  = false;
    zzEOFDone = false;
    zzEndRead = zzStartRead = 0;
    zzCurrentPos = zzMarkedPos = 0;
    zzFinalHighSurrogate = 0;
    yyline = yychar = yycolumn = 0;
    zzLexicalState = YYINITIAL;
    if (zzBuffer.length > ZZ_BUFFERSIZE)
      zzBuffer = new char[ZZ_BUFFERSIZE];
  }


  /**
   * Returns the current lexical state.
   */
  public final int yystate() {
    return zzLexicalState;
  }


  /**
   * Enters a new lexical state
   *
   * @param newState the new lexical state
   */
  public final void yybegin(int newState) {
    zzLexicalState = newState;
  }


  /**
   * Returns the text matched by the current regular expression.
   */
  public final String yytext() {
    return new String( zzBuffer, zzStartRead, zzMarkedPos-zzStartRead );
  }


  /**
   * Returns the character at position <tt>pos</tt> from the 
   * matched text. 
   * 
   * It is equivalent to yytext().charAt(pos), but faster
   *
   * @param pos the position of the character to fetch. 
   *            A value from 0 to yylength()-1.
   *
   * @return the character at position pos
   */
  public final char yycharat(int pos) {
    return zzBuffer[zzStartRead+pos];
  }


  /**
   * Returns the length of the matched text region.
   */
  public final int yylength() {
    return zzMarkedPos-zzStartRead;
  }


  /**
   * Reports an error that occured while scanning.
   *
   * In a wellformed scanner (no or only correct usage of 
   * yypushback(int) and a match-all fallback rule) this method 
   * will only be called with things that "Can't Possibly Happen".
   * If this method is called, something is seriously wrong
   * (e.g. a JFlex bug producing a faulty scanner etc.).
   *
   * Usual syntax/scanner level error handling should be done
   * in error fallback rules.
   *
   * @param   errorCode  the code of the errormessage to display
   */
  private void zzScanError(int errorCode) {
    String message;
    try {
      message = ZZ_ERROR_MSG[errorCode];
    }
    catch (ArrayIndexOutOfBoundsException e) {
      message = ZZ_ERROR_MSG[ZZ_UNKNOWN_ERROR];
    }

    throw new Error(message);
  } 


  /**
   * Pushes the specified amount of characters back into the input stream.
   *
   * They will be read again by then next call of the scanning method
   *
   * @param number  the number of characters to be read again.
   *                This number must not be greater than yylength()!
   */
  public void yypushback(int number)  {
    if ( number > yylength() )
      zzScanError(ZZ_PUSHBACK_2BIG);

    zzMarkedPos -= number;
  }


  /**
   * Resumes scanning until the next regular expression is matched,
   * the end of input is encountered or an I/O-Error occurs.
   *
   * @return      the next token
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  public Token yylex() throws java.io.IOException {
    int zzInput;
    int zzAction;

    // cached fields:
    int zzCurrentPosL;
    int zzMarkedPosL;
    int zzEndReadL = zzEndRead;
    char [] zzBufferL = zzBuffer;
    char [] zzCMapL = ZZ_CMAP;

    int [] zzTransL = ZZ_TRANS;
    int [] zzRowMapL = ZZ_ROWMAP;
    int [] zzAttrL = ZZ_ATTRIBUTE;

    while (true) {
      zzMarkedPosL = zzMarkedPos;

      boolean zzR = false;
      int zzCh;
      int zzCharCount;
      for (zzCurrentPosL = zzStartRead  ;
           zzCurrentPosL < zzMarkedPosL ;
           zzCurrentPosL += zzCharCount ) {
        zzCh = Character.codePointAt(zzBufferL, zzCurrentPosL, zzMarkedPosL);
        zzCharCount = Character.charCount(zzCh);
        switch (zzCh) {
        case '\u000B':
        case '\u000C':
        case '\u0085':
        case '\u2028':
        case '\u2029':
          yycolumn = 0;
          zzR = false;
          break;
        case '\r':
          yycolumn = 0;
          zzR = true;
          break;
        case '\n':
          if (zzR)
            zzR = false;
          else {
            yycolumn = 0;
          }
          break;
        default:
          zzR = false;
          yycolumn += zzCharCount;
        }
      }

      zzAction = -1;

      zzCurrentPosL = zzCurrentPos = zzStartRead = zzMarkedPosL;
  
      zzState = ZZ_LEXSTATE[zzLexicalState];

      // set up zzAction for empty match case:
      int zzAttributes = zzAttrL[zzState];
      if ( (zzAttributes & 1) == 1 ) {
        zzAction = zzState;
      }


      zzForAction: {
        while (true) {
    
          if (zzCurrentPosL < zzEndReadL) {
            zzInput = Character.codePointAt(zzBufferL, zzCurrentPosL, zzEndReadL);
            zzCurrentPosL += Character.charCount(zzInput);
          }
          else if (zzAtEOF) {
            zzInput = YYEOF;
            break zzForAction;
          }
          else {
            // store back cached positions
            zzCurrentPos  = zzCurrentPosL;
            zzMarkedPos   = zzMarkedPosL;
            boolean eof = zzRefill();
            // get translated positions and possibly new buffer
            zzCurrentPosL  = zzCurrentPos;
            zzMarkedPosL   = zzMarkedPos;
            zzBufferL      = zzBuffer;
            zzEndReadL     = zzEndRead;
            if (eof) {
              zzInput = YYEOF;
              break zzForAction;
            }
            else {
              zzInput = Character.codePointAt(zzBufferL, zzCurrentPosL, zzEndReadL);
              zzCurrentPosL += Character.charCount(zzInput);
            }
          }
          int zzNext = zzTransL[ zzRowMapL[zzState] + zzCMapL[zzInput] ];
          if (zzNext == -1) break zzForAction;
          zzState = zzNext;

          zzAttributes = zzAttrL[zzState];
          if ( (zzAttributes & 1) == 1 ) {
            zzAction = zzState;
            zzMarkedPosL = zzCurrentPosL;
            if ( (zzAttributes & 8) == 8 ) break zzForAction;
          }

        }
      }

      // store back cached position
      zzMarkedPos = zzMarkedPosL;

      if (zzInput == YYEOF && zzStartRead == zzCurrentPos) {
        zzAtEOF = true;
            switch (zzLexicalState) {
            case COMMENT: {
              throw new Error("ERROR: " + line_num + ": Lexer: EOF in comment ");
            }
            case 168: break;
            case STRING: {
              throw new Error("ERROR: " + line_num + ": Lexer: Unclosed String");
            }
            case 169: break;
            default:
        return null;
        }
      }
      else {
        switch (zzAction < 0 ? zzAction : ZZ_ACTION[zzAction]) {
          case 1: 
            { throw new Error("ERROR: " + line_num + ": Lexer: invalid character: " + yytext());
            }
          case 57: break;
          case 2: 
            { return token(tok.IDENTIFIER, yytext());
            }
          case 58: break;
          case 3: 
            { return token(tok.INTEGER,    yytext());
            }
          case 59: break;
          case 4: 
            { return token(tok.TYPE,       yytext());
            }
          case 60: break;
          case 5: 
            { return token(tok.MINUS);
            }
          case 61: break;
          case 6: 
            { return token(tok.DOT);
            }
          case 62: break;
          case 7: 
            { line_num += 1;
            }
          case 63: break;
          case 8: 
            { /* ignore */
            }
          case 64: break;
          case 9: 
            { return token(tok.LPAREN);
            }
          case 65: break;
          case 10: 
            { return token(tok.TIMES);
            }
          case 66: break;
          case 11: 
            { string.setLength(0); yybegin(STRING);
            }
          case 67: break;
          case 12: 
            { return token(tok.AT);
            }
          case 68: break;
          case 13: 
            { return token(tok.COLON);
            }
          case 69: break;
          case 14: 
            { return token(tok.COMMA);
            }
          case 70: break;
          case 15: 
            { return token(tok.DIVIDE);
            }
          case 71: break;
          case 16: 
            { return token(tok.EQUALS);
            }
          case 72: break;
          case 17: 
            { return token(tok.LT);
            }
          case 73: break;
          case 18: 
            { return token(tok.LBRACE);
            }
          case 74: break;
          case 19: 
            { return token(tok.PLUS);
            }
          case 75: break;
          case 20: 
            { return token(tok.RBRACE);
            }
          case 76: break;
          case 21: 
            { return token(tok.RPAREN);
            }
          case 77: break;
          case 22: 
            { return token(tok.SEMI);
            }
          case 78: break;
          case 23: 
            { return token(tok.TILDE);
            }
          case 79: break;
          case 24: 
            { 
            }
          case 80: break;
          case 25: 
            { incrementLine();
            }
          case 81: break;
          case 26: 
            { string.append(yytext());
            }
          case 82: break;
          case 27: 
            { throw new Error("ERROR: " + line_num + ": Lexer: Invalid character \"");
            }
          case 83: break;
          case 28: 
            { // always marks the end of a string
                                yybegin(YYINITIAL);
                                return token(tok.STRING, string.toString());
            }
          case 84: break;
          case 29: 
            { throw new Error("ERROR: " + line_num + ": Lexer: NUL in String");
            }
          case 85: break;
          case 30: 
            { yypushback(1); yybegin(YYINITIAL);
            }
          case 86: break;
          case 31: 
            { yybegin(INLINECOMMENT);
            }
          case 87: break;
          case 32: 
            { return token(tok.FI);
            }
          case 88: break;
          case 33: 
            { return token(tok.IF);
            }
          case 89: break;
          case 34: 
            { return token(tok.IN);
            }
          case 90: break;
          case 35: 
            { return token(tok.OF);
            }
          case 91: break;
          case 36: 
            { yybegin(COMMENT); comment_level = 1;
            }
          case 92: break;
          case 37: 
            { return token(tok.RARROW);
            }
          case 93: break;
          case 38: 
            { return token(tok.LARROW);
            }
          case 94: break;
          case 39: 
            { return token(tok.LE);
            }
          case 95: break;
          case 40: 
            { comment_level += 1;
            }
          case 96: break;
          case 41: 
            { comment_level--;  // leaving a level of nested comment
                    if (comment_level == 0) { // if already outside, go back to default processing
                        yybegin(YYINITIAL);
                    }
            }
          case 97: break;
          case 42: 
            { return token(tok.LET);
            }
          case 98: break;
          case 43: 
            { return token(tok.NEW);
            }
          case 99: break;
          case 44: 
            { return token(tok.NOT);
            }
          case 100: break;
          case 45: 
            { return token(tok.CASE);
            }
          case 101: break;
          case 46: 
            { return token(tok.ESAC);
            }
          case 102: break;
          case 47: 
            { return token(tok.ELSE);
            }
          case 103: break;
          case 48: 
            { return token(tok.LOOP);
            }
          case 104: break;
          case 49: 
            { return token(tok.THEN);
            }
          case 105: break;
          case 50: 
            { return token(tok.TRUE);
            }
          case 106: break;
          case 51: 
            { return token(tok.POOL);
            }
          case 107: break;
          case 52: 
            { return token(tok.CLASS);
            }
          case 108: break;
          case 53: 
            { return token(tok.FALSE);
            }
          case 109: break;
          case 54: 
            { return token(tok.WHILE);
            }
          case 110: break;
          case 55: 
            { return token(tok.ISVOID);
            }
          case 111: break;
          case 56: 
            { return token(tok.INHERITS);
            }
          case 112: break;
          default:
            zzScanError(ZZ_NO_MATCH);
        }
      }
    }
  }


}