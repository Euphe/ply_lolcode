# -*- coding: utf-8 -*-
import ply
import ply.lex as lex
import ply.yacc as yacc
import sys


# LEX
keywords = (
    'I', 'HAZ', 'A', 'R', 'ITZ',
    'SUM', 'DIFF', 'PRODUKT', 'QUOSHUNT', 'MOD', 'BIGGR', 'SMALLR', 'BOTH', 'WON', 'EITHER', 'SAEM', 'DIFFRINT', 'ALL', 'ANY',
    'OF', 'AN', 'IT',
    'NOOB', 'WIN', 'FAIL',
    'NOT',
    'O', 'RLY', 'YA', 'NO', 'WAI', 'OIC',
    'VISIBLE',
)

tokens = keywords + (
     'NEWLINE',
     'ID',
     'VARIABLE',
     'INT', 'FLOAT', 'STRING',
     'MINUS',
)

t_RLY = 'RLY?'
t_MINUS = '-'
t_ignore = ' \t?'

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
    r'\n'
    t.lexer.lineno += 1
    return t

def t_error(t):
    print "Illegal character", t.value[0]
    t.lexer.skip(1)

lex.lex(debug=1)

# Parsing rules

variables = { }

def p_program(p):
    '''program : program construct empty
               | construct empty
    '''
    if len(p) == 3:
        construct = p[1]
        p[0] = construct
    elif len(p) == 4:
        p[0] = p[1] + p[2]

def p_construct(p):
    '''construct : construct statement
                 | statement
    '''
    if len(p) == 2 or len(p) == 3 and not p[2]:
        if isinstance(p[1], list):
            p[0] = (p[0] or []) + p[1] 
        if isinstance(p[1], tuple):
            p[0] = [p[1]]
    elif len(p) == 3 and p[2]:
        p[0] = p[1] or []
        p[0] = p[0] + [p[2]]


def p_construct_if(p):
    'construct : expression NEWLINE O RLY NEWLINE YA RLY NEWLINE construct NO WAI NEWLINE construct OIC'
    condition = p[1]
    branch_one = p[9]
    branch_two = p[13]
    if condition:
        p[0] = branch_one
    else:
        p[0] = branch_two

def p_statement_newline(p):
    'statement : NEWLINE'
    p[0] = None

def p_statement(p):
    'statement : command NEWLINE'
    p[0] = p[1]
    eval(p[0])

def p_command_declare(p):
    'command : I HAZ A VARIABLE'
    variables[p[4]] = 'NOOB'

def p_statement_assign(p):
    'statement : VARIABLE R expression'
    variables[p[1]] = p[3]

def p_statement_declare_assign(p):
    'statement : I HAZ A VARIABLE ITZ expression'
    variables[p[4]] = p[6]

def p_command_visible(p):
    '''command : VISIBLE expression'''
    p[0] = ('VISIBLE', p[2])

def p_expression_binop_both_same(p):
    ''' expression : BOTH SAEM expression expression
                   | BOTH SAEM expression AN expression
    '''
    if len(p) == 4:
        p[0] = binop('BOTH SAEM', p[4], p[5])
    elif len(p) == 5:
        p[0] = binop('BOTH SAEM', p[4], p[6])

def p_expression_binop_different(p):
    ''' expression : DIFFRINT expression expression
                   | DIFFRINT expression AN expression
    '''
    if len(p) == 4:
        p[0] = binop(p[1], p[2], p[3])
    elif len(p) == 5:
        p[0] = binop(p[1], p[2], p[4])

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
    p[0] = binop(p[1], p[3], p[4])

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
    p[0] = binop(p[1], p[3], p[5])

def p_expression_unary_op(p):
    ''' expression : MINUS expression
                   | NOT expression
    '''
    p[0] = unary_op(p[1], p[2])

def p_expression_type(p):
    '''expression : WIN
                  | FAIL
                  | INT
                  | FLOAT
                  | STRING
   '''
    p[0] = p[1]

def p_expression_variable(p):
    '''expression : VARIABLE'''
    p[0] = variables[p[1]]

def p_empty(p):
    ''' empty : '''

def p_empty_newline(p):
    ''' empty : NEWLINE'''

def p_error(p):
    print "SYNTAX ERROR AT TOKEN %s" % p

parser = yacc.yacc()

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
    return to_lolcode_type(ops[op](x))

def eval(p):
    if not p:
        return
    #print 'eval p %s' % str([ x for x in p])
    op = p[0]
    if op == 'VISIBLE':
        print(p[1]) 

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
    return to_lolcode_type(binops[op](x, y))


if len(sys.argv) == 2:
    parser.error = 0
    data = open(sys.argv[1]).read()
    print('Parsing and interpreting')
    prog = parser.parse(data)
    print('Final program', prog)
