# Cool-Interpreter
An interpreter for the Classroom Object Oriented Language ([Cool](https://en.wikipedia.org/wiki/Cool_(programming_language))).

## Overview
This is the semester-long project for CS4610 Programming Languages at UVA. The project is an interpreter for the 
Cool programming language. 
The interpreter is broken down into four stages and implemented using four programming languages.

1. Lexing (Implemented in Java using [jflex](http://jflex.de/))
- The Cool source code is tokenized in this stage.
2. Parsing (Implemented in Python using [Ply](http://www.dabeaz.com/ply/ply.html))
- Tokens are parsed into an Abstract Syntax Tree (AST).
3. Type Checking (Implemented in Nodejs)
- Static checks are performed at this stage.
4. Interpreting (Implemented in [Reason](https://facebook.github.io/reason/))
- Program is interpreted and runtime checks are performed at this stage.

## Usage
To execute a Cool program called hello_world.cl, 

```
sh cooler.sh hello_world.cl
```

## Acknowledgement
Thanks my teammate Han Jin (hj5fb@virginia.edu) for implementing this project with me and thanks my professor Kevin Angstadt
(angstadt@virginia.edu) for the general guidance.
