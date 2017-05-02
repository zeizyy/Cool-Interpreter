'''
    This module contains printing methods
'''

FOUT = None


def set_fout(_fout):
    ' setter for file output '
    global FOUT
    FOUT = _fout


def open_file(filename):
    ' open a file as FOUT '
    global FOUT
    FOUT = open(filename, 'w')


def close():
    ' close the file writer '
    FOUT.close()


def write_lines(*messages):
    ' wrapper method for printing '
    for msg in messages:
        FOUT.write(str(msg) + '\n')


# +-----------------------------------------------+
# |                Printers                       |
# +-----------------------------------------------+

def print_list(ast, print_element_funciton):
    ' print list using higher order function '
    FOUT.write(str(len(ast)) + '\n')
    for elem in ast:
        print_element_funciton(elem)


def print_program(ast):
    ' print the whole program '
    print_list(ast, print_class)


def print_class(ast):
    ' print class with no inherits '
    # (p.lineno(1), 'class_noinherits', p[2], None, p[4])
    print_identifier(ast[2])
    write_lines(ast[1])
    print_identifier(ast[3])
    print_list(ast[4], print_feature)


def print_identifier(ast):
    ' print identifier '
    if not ast:
        return
    if isinstance(ast, str):
        write_lines(ast)
    else:
        write_lines(ast[0], ast[1])


def print_formal(ast):
    ' use identifier print to print out formal\'s name and type '
    print_identifier(ast[0]) # name
    print_identifier(ast[1]) # identifier


# =====================| Expression Printers |=====================

OPERATOR_DICT = {
    'equals' : 'eq',
}


def print_binary_expr(ast):
    ' binary operations: (linno, operator name, left operand, right operand) '
    lnno, operator, left, right = ast
    operator = OPERATOR_DICT.get(operator, operator)
    write_lines(lnno, operator)
    print_expr(left)
    print_expr(right)


UNARY_DICT = {
    'tilde': 'negate',
}


def print_unary_expr(ast):
    ' unary expressions: (linno, operator name, operand) '
    lnno, operator, operand = ast
    operator = UNARY_DICT.get(operator, operator)
    write_lines(lnno, operator)
    print_expr(operand)


def print_integer(ast):
    ' integer (lineno, int) '
    write_lines(ast[0], 'integer', ast[1])


def print_string(ast):
    ' string (lineno, string)'
    write_lines(ast[0], 'string', ast[1])


def print_id_in_expr(ast):
    '# print_id_in_expr: (lineno, id)'
    write_lines(ast[0], 'identifier')
    print_identifier(ast[1])


def print_bool(ast):
    '# print_bool: (lineno, id)'
    write_lines(ast[0], ast[1])


def print_assign(ast):
    '# print_assign: (lineno, id, expr)'
    write_lines(ast[0], 'assign')
    print_identifier(ast[1])
    print_expr(ast[2])


def print_if(ast):
    'print_if:  (lineno, if, then, else)'
    write_lines(ast[0], 'if')
    print_expr(ast[1])
    print_expr(ast[2])
    print_expr(ast[3])


def print_while(ast):
    'print_while: (lineno, termination, action)'
    write_lines(ast[0], 'while')
    print_expr(ast[1])
    print_expr(ast[2])


def print_block(ast):
    ' print block (lineno, block) '
    write_lines(ast[0], 'block')
    print_list(ast[1], print_expr)


def print_new(ast):
    ' print new using tuple decomposition '
    lineno, tpe = ast
    write_lines(ast[0], 'new')
    print_identifier(tpe)


def print_isvoid(ast):
    ' print isvoid using tuple decomposition '
    lineno, exp = ast
    write_lines(ast[0], 'isvoid')
    print_expr(exp)


def print_let(ast):
    ' print let using tuple decomposition '
    lineno, exp_list, exp = ast
    write_lines(lineno, 'let')
    print_list(exp_list, print_each_let)
    print_expr(exp)


def print_each_let(ast):
    ' print every let expr using tuple decomposition '
    lineno, with_init, identifier, tpe, exp = ast
    if (with_init == 'attribute_init'):
        write_lines('let_binding_init')
    else:
        write_lines('let_binding_no_init')
    print_identifier(identifier)
    print_identifier(tpe)
    if (with_init == 'attribute_init'):
        print_expr(exp)


def print_case_element(ast):
    ' print case sub expr using tuple decomposition '
    identifier, type, exp = ast
    print_identifier(identifier)
    print_identifier(type)
    print_expr(exp)


def print_case(ast):
    ' print case using tuple decomposition '
    lineno, value, caselist = ast
    write_lines(lineno, 'case')
    print_expr(value)
    print_list(caselist, print_case_element)


def print_static_dispatch(ast):
    ' print static dispatch using tuple decomposition '
    lineno, expr, typ, method, param_list = ast
    write_lines(lineno, 'static_dispatch')
    print_expr(expr)
    print_identifier(typ)
    print_identifier(method)
    print_list(param_list, print_expr)


def print_dynamic_dispatch(ast):
    ' print dynamic dispatch using tuple decomposition '
    lineno, assgined, method, param_list = ast
    write_lines(lineno, 'dynamic_dispatch')
    print_expr(assgined)
    print_identifier(method)
    print_list(param_list, print_expr)


def print_self_dispatch(ast):
    ' print self dispatch using tuple decomposition '
    lineno, caller, expr_list = ast
    write_lines(lineno, 'self_dispatch')
    print_identifier(caller)
    print_list(expr_list, print_expr)


def print_paren(ast):
    ' print parenthesis enclosed expr using tuple decomposition '
    _, exp = ast
    print_expr(exp)

EXPR_PRINTERS = {
    ''' collection of all expression printers to allow polymorphisim '''
    'binary'           : print_binary_expr,
    'assign'           : print_assign,
    'dynamic_dispatch' : print_dynamic_dispatch,
    'static_dispatch'  : print_static_dispatch,
    'self_dispatch'    : print_self_dispatch,
    'condition'        : print_if,
    'while'            : print_while,
    'block'            : print_block,
    'let'              : print_let,
    'case'             : print_case,
    'paren'            : print_paren,
    'new'              : print_new,
    'isvoid'           : print_isvoid,
    'unary'            : print_unary_expr,
    'integer'          : print_integer,
    'string'           : print_string,
    'identifier'       : print_id_in_expr,
    'bool'             : print_bool
}


def print_expr(ast):
    ' print an expression using the first component, an str, for polymorphism '
    expr = ast[0]
    EXPR_PRINTERS[expr](ast[1:])


# =========================| Feature Printers |===========================


def print_attribute_no_init(ast):
    ' print attribute without init expression using tuple decomposition '
    _, attr, identifier, type, _ = ast
    write_lines(attr)
    print_identifier(identifier)
    print_identifier(type)


def print_attribute_init(ast):
    ' print attribute with init expression using tuple decomposition '
    _, attr, identifier, type, init_expr = ast
    write_lines(attr)
    print_identifier(identifier)
    print_identifier(type)
    print_expr(init_expr)


def print_method(ast):
    ' print a method using tuple decomposition '
    _, attr, identifier, type, formallist, exp = ast
    write_lines(attr)
    print_identifier(identifier)
    print_list(formallist, print_formal)
    print_identifier(type)
    print_expr(exp)


FEATURE_PRINTERS = {
    ''' collection of feature printers to allow polymorphism '''
    'attribute_no_init' : print_attribute_no_init,
    'attribute_init'    : print_attribute_init,
    'method'            : print_method
}


def print_feature(ast):
    ' Using the second position in ast for polymorphic call to a feature printer '
    FEATURE_PRINTERS[ast[1]](ast)

