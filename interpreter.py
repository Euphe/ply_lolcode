# -*- coding: utf-8 -*-
import ply
import ply.lex as lex
import ply.yacc as yacc
import sys


# LEX
keywords = (
    'I', 'HAZ', 'A',
    'R',
    'SUM', 'OF', 'AN',
    'VISIBLE',
)

tokens = keywords + (
     'NUMBER',
     'NEWLINE',
     'ID',
     'VARIABLE'
)

t_ignore = ' \t'

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    if t.value in keywords:
        t.type = t.value
    else:
        t.type = 'VARIABLE'
    return t

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)    
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
    '''program : program statement
               | statement
    '''
    if len(p) == 2 and p[1]:
        statement = p[1]
        p[0] = [statement]
    elif len(p) ==3:
        program = p[1]
        statement = p[2]
        p[0] = []
        if program:
            p[0].append(program)
        if statement:
            p[0].append(statement)


def p_statement_newline(p):
    '''statement : NEWLINE'''
    pass

def p_statement(p):
    'statement : command NEWLINE'
    p[0] = p[1]

def p_command_declare(p):
    'command : I HAZ A VARIABLE'
    variables[p[4]] = 'NOOB'

def p_statement_assign(p):
    'statement : VARIABLE R expression'
    variables[p[1]] = p[3]

def p_command_visible(p):
    '''command : VISIBLE expression'''
    p[0] = ('PRINT', p[2])
    eval(p[0])

def p_expression_sum(p):
    '''expression : SUM OF expression AN expression'''
    p[0] = p[3] + p[5]

def p_expression_number(p):
    '''expression : NUMBER'''
    p[0] = p[1]

def p_expression_variable(p):
    '''expression : VARIABLE'''
    p[0] = variables[p[1]]

def p_error(p):
    print "SYNTAX ERROR AT TOKEN %s" % p


parser = yacc.yacc()

# Evaluation
def eval(p):
    #print('eval', p)
    if p[0] == 'PRINT':
        print(p[1])
    else:
        raise RuntimeError('Unknown operation %s' % (p))

if len(sys.argv) == 2:
    parser.error = 0

    data = open(sys.argv[1]).read()
    print('Parsing and interpreting')
    prog = parser.parse(data)
    print(prog)
    if not prog: raise SystemExit