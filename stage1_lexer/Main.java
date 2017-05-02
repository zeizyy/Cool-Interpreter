import java.io.*;

public class Main {
    public static void main(String[] args) {
        /* check args */
        if (args.length < 1) {
            System.out.println("Not enough arguments");
            System.exit(1);
        }
        String inputFilename = args[0];

        /* read input file char by char */
        StringBuilder inputContent = new StringBuilder();
        try (BufferedInputStream fileStream = new BufferedInputStream(new FileInputStream(new File(inputFilename)))) {
            char c = 'a';
            while (fileStream.available() > 0) {
                inputContent.append((char)(fileStream.read()));
            }
        } catch (Exception e) {
            System.out.println("Input File not found.");
            System.out.println(e.getMessage());
        }

        /* feed input file to the Lexer */
        Lexer l = new Lexer(new StringReader(inputContent.toString()));
        StringBuilder outputBuilder = new StringBuilder();
        try {
            Token t;
            /* write each token to the string builder */
            while ((t = l.yylex()) != null) {
                outputBuilder.append(t.toString());
                outputBuilder.append("\n");
            }
        } catch (Error e) {
            /* catch any error from the Lexer */
            System.out.println(e.getMessage());
            System.exit(1);
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }

        /* hard code the check for unterminated comment */
        if (l.comment_level > 0)
            System.out.println("ERROR: " + l.line_num + ": Lexer: " + "EOF in (* comment *)");

        /* write the Lexer output to a filename.cl-lex if there is no lexer error. */
        try (BufferedWriter filewriter = new BufferedWriter(new FileWriter(args[0] + "-lex"))) {
            filewriter.write(outputBuilder.toString());
        } catch (Exception e) {
            System.out.println("Something is wrong when writing output file.");
            System.out.println(e.getMessage());
        }
    }


}
