# -*- coding: utf-8 -*-
import ply
import ply.lex as lex
import ply.yacc as yacc
import sys


# LEX
keywords = (
    'HAI', 'KTHXBYE',
    'I', 'HAS', 'A', 'R', 'ITZ',
    'SUM', 'DIFF', 'PRODUKT', 'QUOSHUNT', 'MOD', 'BIGGR', 'SMALLR', 'BOTH', 'WON', 'EITHER', 'SAEM', 'DIFFRINT', 'ALL', 'ANY',
    'OF', 'AN', 'IT',
    'NOOB', 'WIN', 'FAIL',
    'NOT',
    'O', 'RLY', 'YA', 'NO', 'WAI', 'OIC', 'MEBBE',
    'VISIBLE',
)

tokens = keywords + (
     'NEWLINE',
     'ID',
     'VARIABLE',
     'INT', 'FLOAT', 'STRING',
     'MINUS',
     'COMMENT', 'MULTILINE_COMMENT'
)

t_MINUS = '-'
t_ignore = ' \t?'

def t_COMMENT(t):
    r'BTW .+'

def t_MULTILINE_COMMENT(t):
    r'OBTW(.|[\r\n])*?TLDR'

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    if t.value in keywords:
        t.type = t.value
    else:
        t.type = 'VARIABLE'
    return t

def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_STRING(t):
    r'\".*?\"'
    t.value = t.value[1:-1]
    return t

def t_NEWLINE(t):
    r'(\n+|,)'
    t.lexer.lineno += 1
    return t

def t_error(t):
    print "Illegal character", t.value[0]
    t.lexer.skip(1)

lex.lex(debug=1)

# Parsing rules

precedence = (('left','SUM','DIFF'),('left','PRODUKT','QUOSHUNT'),('right', 'MINUS'),)

variables = { }

cur_line = 0

def p_file(p):
    '''file : HAI FLOAT NEWLINE program KTHXBYE
            | HAI FLOAT NEWLINE program KTHXBYE NEWLINE'''
    p[0] = p[4]

def p_program(p):
    '''program : program construct
               | construct
    '''
    if len(p) == 2:
        construct = p[1]
        p[0] = construct
    elif len(p) == 3:
        if not p[1] and not p[2]:
            p[0] = None
        elif p[1] and not p[2]:
            p[0] = p[1]
        elif not p[1] and p[2]:
            p[0] = p[2]
        else:
            if not p[2] in p[1]:
                # We reach this point if a construct has been maximally reduced to a list of statements
                # Thus we execute each statement, but only if it's on a line after the current line number
                construct = p[2]
                if isinstance(construct, list):
                    eval_construct(construct)
                if isinstance(p[2], tuple):
                    p[2] = [p[2]]
                p[0] = p[1] + (p[2] or [])

def p_construct(p):
    '''construct : construct empty construct
                 | statement empty
    '''
    if len(p) == 3 or len(p) == 4 and not p[3]:
        if isinstance(p[1], list):
            p[0] = (p[0] or []) + p[1] 
        if isinstance(p[1], tuple):
            p[0] = [p[1]]        
    elif len(p) == 4 and p[3]:
        if isinstance(p[1], tuple):
            p[1] = [p[1]]
        if isinstance(p[3], tuple):
            p[3] = [p3]
        p[0] = p[1] or []
        if not p[3] in p[0]:
            p[0] = p[0] + p[3]

def p_construct_if_else(p):
    'construct : expression NEWLINE O RLY NEWLINE YA RLY NEWLINE construct NO WAI NEWLINE construct OIC'
    condition = p[1]
    if_true = p[9]
    else_ = p[13]
    p[0] = (p.lineno(2), 'IF', condition, if_true, None, else_)

def p_construct_if(p):
    'construct : expression NEWLINE O RLY NEWLINE YA RLY NEWLINE construct OIC'
    condition = p[1]
    if_true = p[9]
    p[0] = (p.lineno(2), 'IF', condition, if_true, None, None)

def p_construct_if_elif(p):
    'construct : expression NEWLINE O RLY NEWLINE YA RLY NEWLINE construct elifs NO WAI NEWLINE construct OIC'
    condition = p[1]
    if_true = p[9]
    elif_blocks = p[10]
    else_ = p[14]
    p[0] = (p.lineno(2), 'IF', condition, if_true, elif_blocks, else_)

def p_elif(p):
    '''
    elifs : elif
          | elifs elif
    '''
    if len(p) == 2 and p[1]:
        p[0] = [p[1]]
    elif len(p) == 3:
        p[0] = p[1] + [p[2]]


def p_elifs(p):
    '''
    elif : MEBBE expression NEWLINE construct
    '''
    p[0] = (p.lineno(1), 'ELIF', p[2], p[4])

def p_statement(p):
    'statement : command NEWLINE'
    p[0] = p[1]

def p_command_declare(p):
    'command : I HAS A VARIABLE'
    p[0] = (p.lineno(1), 'DECLARE', p[4], None)

def p_command_assign(p):
    'command : VARIABLE R expression'
    p[0] = (p.lineno(1), 'ASSIGN', p[1], p[3])

def p_command_declare_assign(p):
    'statement : I HAS A VARIABLE ITZ expression'
    p[0] = (p.lineno(1), 'DECLARE', p[4], p[6])

def p_command_visible(p):
    'command : VISIBLE expression'
    p[0] = (p.lineno(1), 'VISIBLE', p[2])

def p_expression_binop_both_same(p):
    ''' expression : BOTH SAEM expression expression
                   | BOTH SAEM expression AN expression
    '''
    if len(p) == 4:
        p[0] = (p.lineno(1), 'BINOP', 'BOTH SAEM', p[4], p[5])
    elif len(p) == 5:
        p[0] = (p.lineno(1), 'BINOP', 'BOTH SAEM', p[4], p[6])

def p_expression_binop_different(p):
    ''' expression : DIFFRINT expression expression
                   | DIFFRINT expression AN expression
    '''
    if len(p) == 4:
        p[0] = (p.lineno(1), 'BINOP', p[1], p[2], p[3])
    elif len(p) == 5:
        p[0] = (p.lineno(1), 'BINOP', p[1], p[2], p[4])

def p_expression_binop(p):
    ''' expression  : SUM OF expression expression
                    | DIFF OF expression expression
                    | PRODUKT OF expression expression
                    | QUOSHUNT OF expression expression
                    | MOD OF expression expression
                    | BOTH OF expression expression
                    | EITHER OF expression expression
                    | WON OF expression expression
                    | SMALLR OF expression expression
                    | BIGGR OF expression expression
    '''
    p[0] = (p.lineno(1), 'BINOP', p[1], p[3], p[4])

def p_expression_binop_an(p):
    ''' expression  : SUM OF expression AN expression
                    | DIFF OF expression AN expression
                    | PRODUKT OF expression AN expression
                    | QUOSHUNT OF expression AN expression
                    | MOD OF expression AN expression
                    | BOTH OF expression AN expression
                    | EITHER OF expression AN expression
                    | WON OF expression AN expression
                    | SMALLR OF expression AN expression
                    | BIGGR OF expression AN expression
    '''
    p[0] = (p.lineno(1), 'BINOP', p[1], p[3], p[5])

def p_expression_unary_op(p):
    ''' expression : MINUS expression
                   | NOT expression
    '''
    p[0] = (p.lineno(1), 'UNARY', p[1], p[2])

def p_expression_int(p):
    'expression : INT'
    p[0] = (p.lineno(1), 'INT', p[1])

def p_expression_float(p):
    'expression : FLOAT'
    p[0] = (p.lineno(1), 'FLOAT', p[1])

def p_expression_string(p):
    'expression : STRING'
    p[0] = (p.lineno(1), 'STRING', p[1])

def p_expression_true(p):
    'expression : WIN'
    p[0] = (p.lineno(1), 'WIN', True)

def p_expression_false(p):
    'expression : FAIL'
    p[0] = (p.lineno(1), 'FAIL', False)

def p_expression_variable(p):
    '''expression : VARIABLE'''
    p[0] = (p.lineno(1), 'VAR', p[1])

def p_empty(p):
    ''' empty : '''
    p[0] = None

def p_empty_newline(p):
    ''' empty : NEWLINE'''
    p[0] = None

def p_error(p):
    print "SYNTAX ERROR AT TOKEN %s" % p

parser = yacc.yacc()

def format_lolcode_string(text):
    text = text.replace(':)', '\n') \
            .replace(':>', '\t') \
            .replace(':o', '\g') \
            .replace(':"', '"') \
            .replace('::', ':')
    # Todo unicode code points
    # Todo variable substitution
    return text

def from_lolcode_type(x):
    if x == 'FAIL':
        return False
    elif x == 'WIN':
        return True
    elif x == 'NOOB':
        return None
    return x

def to_lolcode_type(x):
    if x is False:
        return 'FAIL'
    elif x is True:
        return 'WIN'
    elif x is None:
        return 'NOOB'
    return x

def unary_op(op, x):
    x = from_lolcode_type(x)
    ops = {
        'NOT': lambda x: not x,
        '-': lambda x: -x,
    }
    return ops[op](x)

def eval_construct(construct):
    for st in construct:
        if st[0] > cur_line:
            eval(st)

def eval(p):
    global cur_line
    if not p:
        return
    # print('eval', p)
    if not isinstance(p, tuple):
        return p
    lineno = p[0]
    if lineno < cur_line:
        raise(Exception('What? You are at line ' + str(cur_line)))
    op = p[1]
    arg = p[2]
    if op in ['INT', 'FLOAT', 'STRING', 'WIN', 'FAIL']:
        return p[2]
    elif op == 'VAR':
        return variables[eval(arg)]
    elif op in ['DECLARE', 'ASSIGN']:
        variables[eval(arg)] = eval(p[3])
    elif op == 'VISIBLE':
        text = eval(arg)
        if isinstance(text, str):
            text = format_lolcode_string(text)
        text = to_lolcode_type(text)
        print(text)
    elif op == 'BINOP':
        operation = p[2]
        x = eval(p[3])
        y = eval(p[4])
        return binop(operation, x, y)
    elif op == 'UNARY':
        operation = p[2]
        arg = eval(p[3])
        return unary_op(operation, arg)
    elif op == 'IF':
        expr = eval(p[2])
        if_true_construct = p[3]
        elifs = p[4]
        else_construct = p[5]
        if expr and if_true_construct:
            eval_construct(if_true_construct)
        else:
            eval_construct(else_construct)


    else:
        raise(Exception('Unknown operation: ' +str(p)))

    cur_line = lineno

def binop(op, x, y):
    x = from_lolcode_type(x)
    y = from_lolcode_type(y)
    binops = {
        'SUM': lambda x, y: x + y,
        'DIFF': lambda x, y: x - y,
        'PRODUKT': lambda x, y: x * y,
        'QUOSHUNT': lambda x, y: x / y,
        'MOD': lambda x, y: x % y,
        'BIGGR': lambda x, y: max(x, y),
        'SMALLR': lambda x, y: min(x, y),
        'BOTH SAEM': lambda x, y: x == y,
        'DIFFRINT': lambda x, y: x != y,
        'EITHER': lambda x, y: x or y,
        'BOTH': lambda x, y: x and y,
        'WON': lambda x, y: not (x == y),
    }
    return binops[op](x, y)


if len(sys.argv) == 2:
    parser.error = 0
    data = open(sys.argv[1]).read()
    print('Parsing and interpreting')
    prog = parser.parse(data)
    #print('Final program', prog)
