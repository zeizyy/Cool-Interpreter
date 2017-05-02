import sys
from ply import yacc as yacc
from ply.lex import LexToken
from op_dict import get_symbol


# =========================| Handle Input |=========================
filename = sys.argv[1]
with open(filename, 'r') as file:
    lines_striped = [ line.rstrip('\n').rstrip('\r') for line in file.readlines()]

pa2_tokens = []
lines_striped_iter = iter(lines_striped)

# read list and covert to tuple
while True:
    line_number = next(lines_striped_iter, None)
    if not line_number:
        break
    token_type = next(lines_striped_iter)
    token_lexeme = token_type
    if token_type in ('integer', 'identifier', 'string', 'type'):
        token_lexeme = next(lines_striped_iter)
    pa2_tokens.append((line_number, token_type.upper(), token_lexeme))

tokens_iter = iter(pa2_tokens)

# ========================| convert token tuple to PA2Lexer object |==================
class PA2Lexer(object):
    def token(self):
        cur_token = next(tokens_iter, None)
        if not cur_token:
            return None
        line, token_type, lexeme = cur_token
        tok = LexToken()
        tok.type = token_type
        tok.value = lexeme
        tok.lineno = line
        tok.lexpos = 0
        return tok

pa2_lexer = PA2Lexer()


#======================| defining the Parser |=======================
tokens = (
    'AT',
    'CASE',
    'CLASS',
    'COLON',
    'COMMA',
    'DIVIDE',
    'DOT',
    'ELSE',
    'EQUALS',
    'ESAC',
    'FALSE',
    'FI',
    'IDENTIFIER',
    'IF',
    'IN',
    'INHERITS',
    'INTEGER',
    'ISVOID',
    'LARROW',
    'LBRACE',
    'LE',
    'LET',
    'LOOP',
    'LPAREN',
    'LT',
    'MINUS',
    'NEW',
    'NOT',
    'OF',
    'PLUS',
    'POOL',
    'RARROW',
    'RBRACE',
    'RPAREN',
    'SEMI',
    'STRING',
    'THEN',
    'TILDE',
    'TIMES',
    'TRUE',
    'TYPE',
    'WHILE',
    )

precedence = (
    ('left', 'LARROW'),
    ('nonassoc', 'NOT'),
    ('nonassoc', 'LE', 'LT', 'EQUALS'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('nonassoc', 'ISVOID'),
    ('nonassoc', 'TILDE'),
    ('left', 'AT'),
    ('left', 'DOT'),
)

def p_program_class(p):
    'program : class SEMI'
    p[0] = [p[1]]

def p_program_classlist(p):
    'program : class SEMI program'
    p[0] = [p[1]] + p[3]

def p_class_noninherit(p):
    'class : CLASS type LBRACE featurelist RBRACE'
    p[0] = (p.lineno(1), 'no_inherits', p[2], None, p[4])

def p_class_inherits(p):
    'class : CLASS type INHERITS type LBRACE featurelist RBRACE'
    p[0] = (p.lineno(1), 'inherits', p[2], p[4], p[6])

def p_type(p):
    'type : TYPE'
    p[0] = (p.lineno(1), p[1])

def p_identifier(p):
    'identifier : IDENTIFIER'
    p[0] = (p.lineno(1), p[1])

def p_featurelist_none(p):
    'featurelist : '
    p[0] = []

def p_featurelist_some(p):
    'featurelist : feature SEMI featurelist'
    p[0] = [p[1]] + p[3]

def p_feature_attribute(p):
    '''feature_attribute : feature_attribute_no_init
                         | feature_attribute_init'''
    p[0] = p[1]

def p_feature_attributenoinit(p):
    'feature_attribute_no_init : identifier COLON type'
    p[0] = (p.lineno(2), 'attribute_no_init', p[1], p[3], None)

def p_feature_attributeinit(p):
    'feature_attribute_init : identifier COLON type LARROW expr'
    p[0] = (p.lineno(2), 'attribute_init', p[1], p[3], p[5])

def p_feature_method(p):
    '''feature_method : p_feature_method_noformal
                      | p_feature_method_formal'''
    p[0] = p[1]

def p_feature_method_noformal(p):
    'p_feature_method_noformal : identifier LPAREN RPAREN COLON type LBRACE expr RBRACE'
    p[0] = (p.lineno(2), 'method', p[1], p[5], [], p[7])

def p_feature_method_formal(p):
    'p_feature_method_formal : identifier LPAREN formallist RPAREN COLON type LBRACE expr RBRACE'
    p[0] = (p.lineno(2), 'method', p[1], p[6], p[3], p[8])

def p_feature(p):
    '''feature : feature_attribute
               | feature_method'''
    p[0] = p[1]

def p_string(p):
    '''string : STRING'''
    p[0] = (p.lineno(1), p[1])

def p_formallist_one(p):
    'formallist : formal'
    p[0] = [p[1]]

def p_formallist_some(p):
    'formallist : formal COMMA formallist'
    p[0] = [p[1]] + p[3]

def p_formal(p):
    'formal : identifier COLON type'
    p[0] = (p[1], p[3])

def p_expr_assign(p):
    '''expr : identifier LARROW expr'''
    p[0] = ('assign', p.lineno(2), p[1], p[3])

def p_expr_list_one(p):
    '''expr_list : expr'''
    p[0] = [p[1]]

def p_expr_list_some(p):
    '''expr_list : expr COMMA expr_list'''
    p[0] = [p[1]] + p[3]

def p_expr_dynamic_dispatch(p):
    '''expr : expr DOT function_call'''
    lineno, method, params = p[3]
    p[0] = ('dynamic_dispatch', p.lineno(2), p[1], method, params)

def p_expr_static_dispatch(p):
    '''expr : expr AT type DOT function_call'''
    lineno, method, params = p[5]
    p[0] = ('static_dispatch', p.lineno(2), p[1], p[3], method, params)

def p_expr_self_dispatch(p):
    '''expr : function_call'''
    lineno, method, params = p[1]
    p[0] = ('self_dispatch', lineno, method, params)

def p_func_call(p):
    '''function_call : function_call_noexpr
                     | function_call_expr'''
    p[0] = p[1]

def p_func_call_noexpr(p):
    '''function_call_noexpr : identifier LPAREN RPAREN'''
    p[0] = (p.lineno(2), p[1], [])

def p_func_call_expr(p):
    '''function_call_expr : identifier LPAREN expr_list RPAREN'''
    p[0] = (p.lineno(2), p[1], p[3])

def p_expr_condition(p):
    '''expr : IF expr THEN expr ELSE expr FI'''
    p[0] = ('condition', p.lineno(1), p[2], p[4], p[6])

def p_expr_while(p):
    '''expr : WHILE expr LOOP expr POOL'''
    p[0] = ('while', p.lineno(1), p[2], p[4])

def p_expr_block(p):
    '''expr : LBRACE block RBRACE'''
    p[0] = ('block', p.lineno(1), p[2])

def p_block_single(p):
    '''block : expr SEMI'''
    p[0] = [p[1]]

def p_block_some(p):
    '''block : expr SEMI block'''
    p[0] = [p[1]] + p[3]

def p_expr_let(p):
    '''expr : LET feature_attr_list IN expr'''
    p[0] = ('let', p.lineno(1), p[2], p[4])

def p_feature_attr_list_single(p):
    '''feature_attr_list : feature_attribute'''
    p[0] = [p[1]]

def p_feature_attr_list_some(p):
    '''feature_attr_list : feature_attribute COMMA feature_attr_list'''
    p[0] = [p[1]] + p[3]

def p_expr_case(p):
    '''expr : CASE expr OF case_list ESAC'''
    p[0] = ('case', p.lineno(1), p[2], p[4])

def p_case_item(p):
    '''case_item : identifier COLON type RARROW expr SEMI'''
    p[0] = (p[1], p[3], p[5])

def p_case_list_single(p):
    '''case_list : case_item'''
    p[0] = [p[1]]

def p_case_list_some(p):
    '''case_list : case_item case_list'''
    p[0] = [p[1]] + p[2]

def p_new(p):
    '''expr : NEW type'''
    p[0] = ('new', p.lineno(1), p[2])

def p_isvoid(p):
    '''expr : ISVOID expr'''
    p[0] = ('isvoid', p.lineno(1), p[2])

def p_binary_expr_sum(p):
    '''expr : expr PLUS expr
                    | expr MINUS expr
                    | expr TIMES expr
                    | expr DIVIDE expr
                    | expr EQUALS expr
                    | expr LT expr
                    | expr LE expr'''
    p[0] = ('binary', p.lineno(2), p[2], p[1], p[3])

def p_unary_expr(p):
    '''expr : TILDE expr
                  | NOT expr'''
    p[0] = ('unary', p.lineno(1), p[1], p[2])

def p_expr_paren(p):
    '''expr : LPAREN expr RPAREN'''
    p[0] = ('paren', p.lineno(1), p[2])

def p_expr_id(p):
    '''expr : identifier'''
    p[0] = ('identifier', p[1][0], p[1])

def p_expr_integer(p):
    'expr : integer'
    p[0] = ('integer', p[1][0], p[1][1])

def p_integer(p):
    'integer : INTEGER'
    p[0] = (p.lineno(1), p[1])

def p_expr_string(p):
    'expr : string'
    p[0] = ('string', p[1][0], p[1][1])

def p_expr_bool(p):
    '''expr : TRUE
            | FALSE'''
    p[0] = ('bool', p.lineno(1), p[1])

def p_error(p):
    if p:
        lineno, lexeme = p.lineno, get_symbol(p.value)
    else:
        last_tok = pa2_tokens[-1]
        lineno, lexeme = last_tok[0], get_symbol(last_tok[2])
    print("ERROR: {}: Parser: syntax error near {}".format(lineno, lexeme))
    sys.exit(1)




# ============================| build parser |========================
parser = yacc.yacc()
ast = parser.parse(lexer=pa2_lexer)

# output the cl-ast file
ast_filename = filename[:-4] + '-ast'
fout = open(ast_filename, 'w')

# wrapper method for print line
def write_lines(*messages):
    for msg in messages:
        fout.write(str(msg) + '\n')

# ============================| Defining Printers |========================

def print_program(ast):
    print_list(ast, print_class)

def print_list(ast, print_element_funciton):
    fout.write(str(len(ast)) + '\n')
    for elem in ast:
        print_element_funciton(elem)

def print_class(ast):
    # (p.lineno(1), 'class_noinherits', p[2], None, p[4])
    print_identifier(ast[2])
    write_lines(ast[1])
    print_identifier(ast[3])
    print_list(ast[4], print_feature)

def print_identifier(ast):
    if not ast:
        return
    if isinstance(ast, str):
        write_lines(ast)
    else:
        write_lines(ast[0], ast[1])

# ===================== | Expression Printers | ====================

operator_dict = {
    'equals' : 'eq',
}

def print_binary_expr(ast):
    # (linno, operator name, left operand, right operand)
    lnno, op, left, right = ast
    op = operator_dict.get(op, op)
    write_lines(lnno, op)
    print_expr(left)
    print_expr(right)

unary_dict = {
    'tilde': 'negate',
}

def print_unary_expr(ast):
    # (linno, operator name, operand)
    lnno, op, operand = ast
    op = unary_dict.get(op, op)
    write_lines(lnno, op)
    print_expr(operand)

def print_integer(ast):
    # (lineno, int)
    write_lines(ast[0], 'integer', ast[1])

def print_string(ast):
    # (lineno, string)
    write_lines(ast[0], 'string', ast[1])

def print_id_in_expr(ast):
    # (lineno, id)
    write_lines(ast[0], 'identifier')
    print_identifier(ast[1])

def print_bool(ast):
    # (lineno, id)
    write_lines(ast[0], ast[1])

def print_assign(ast):
    # (lineno, id, expr)
    write_lines(ast[0], 'assign')
    print_identifier(ast[1])
    print_expr(ast[2])

def print_if(ast):
    # (lineno, if, then, else)
    write_lines(ast[0], 'if')
    print_expr(ast[1])
    print_expr(ast[2])
    print_expr(ast[3])

def print_while(ast):
    # (lineno, termination, action)
    write_lines(ast[0], 'while')
    print_expr(ast[1])
    print_expr(ast[2])

def print_block(ast):
    # (lineno, block)
    write_lines(ast[0], 'block')
    print_list(ast[1], print_expr)

def print_new(ast):
    lineno, tpe = ast
    write_lines(ast[0], 'new')
    print_identifier(tpe)

def print_isvoid(ast):
    lineno, exp = ast
    write_lines(ast[0], 'isvoid')
    print_expr(exp)

def print_let(ast):
    lineno, exp_list, exp = ast
    write_lines(lineno, 'let')
    print_list(exp_list, print_each_let)
    print_expr(exp)

def print_each_let(ast):
    lineno, with_init, identifier, tpe, exp = ast
    # write_lines(ast[0])
    if (with_init == 'attribute_init'):
        write_lines('let_binding_init')
    else:
        write_lines('let_binding_no_init')
    print_identifier(identifier)
    print_identifier(tpe)
    if (with_init == 'attribute_init'):
        print_expr(exp)

def print_case_element(ast):
    identifier, type, exp = ast
    print_identifier(identifier)
    print_identifier(type)
    print_expr(exp)

def print_case(ast):
    lineno, value, caselist = ast
    write_lines(lineno, 'case')
    print_expr(value)
    print_list(caselist, print_case_element)

def print_static_dispatch(ast):
    lineno, expr, typ, method, param_list = ast
    write_lines(lineno, 'static_dispatch')
    print_expr(expr)
    print_identifier(typ)
    print_identifier(method)
    print_list(param_list, print_expr)

def print_dynamic_dispatch(ast):
    lineno, assgined, method, param_list = ast
    write_lines(lineno, 'dynamic_dispatch')
    print_expr(assgined)
    print_identifier(method)
    print_list(param_list, print_expr)

def print_self_dispatch(ast):
    lineno, caller, expr_list = ast
    write_lines(lineno, 'self_dispatch')
    print_identifier(caller)
    print_list(expr_list, print_expr)

def print_paren(ast):
    lineno, exp = ast
    print_expr(exp)

expr_printers = {
    'binary' : print_binary_expr,
    'assign' : print_assign,
    'dynamic_dispatch' : print_dynamic_dispatch,
    'static_dispatch' : print_static_dispatch,
    'self_dispatch' : print_self_dispatch,
    'condition' : print_if,
    'while' : print_while,
    'block' : print_block,
    'let' : print_let,
    'case' : print_case,
    'paren' : print_paren,
    'new' : print_new,
    'isvoid' : print_isvoid,
    'unary'  : print_unary_expr,
    'integer': print_integer,
    'string' : print_string,
    'identifier' : print_id_in_expr,
    'bool'   : print_bool
}

def print_expr(ast):
    expr = ast[0]
    expr_printers[expr](ast[1:])

# ================| Feature Printers |===================

def print_attribute_no_init(ast):
    _, attr, identifier, type, _ = ast
    write_lines(attr)
    print_identifier(identifier)
    print_identifier(type)

def print_attribute_init(ast):
    _, attr, identifier, type, init_expr = ast
    write_lines(attr)
    print_identifier(identifier)
    print_identifier(type)
    print_expr(init_expr)

def print_method(ast):
    _, attr, identifier, type, formallist, exp = ast
    write_lines(attr)
    print_identifier(identifier)
    print_list(formallist, print_formal)
    print_identifier(type)
    print_expr(exp)

def print_formal(ast):
    print_identifier(ast[0]) # name
    print_identifier(ast[1]) # identifier

feature_printers = {
    'attribute_no_init' : print_attribute_no_init,
    'attribute_init' : print_attribute_init,
    'method' : print_method
}

def print_feature(ast):
    feature_printers[ast[1]](ast)


# ==================== write the output ========================

print_program(ast)

fout.close()