file_name="hello-world.cl"

# compile lexer (Java)
stage1_lexer/jflex-1.6.1/bin/jflex -q stage1_lexer/Lexer.flex
javac stage1_lexer/Main.java stage1_lexer/Lexer.java stage1_lexer/tok.java stage1_lexer/Token.java

# compile parser (Python) skipped

# compile type checker (JS) skipped

# compile interpreter (Reason)
cd stage4_interpreter
rebuild -ocamlc 'ocamlc unix.cma str.cma' -ocamlopt 'ocamlopt unix.cmxa str.cmxa' -quiet main.native
cd ..

java -cp stage1_lexer Main "$file_name"

python stage2_parser/main.py "$file_name-lex"

node stage3_type_checker/main.js "$file_name-ast"

./stage4_interpreter/main.native "$file_name-type"


