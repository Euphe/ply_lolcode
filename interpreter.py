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
    'VISIBLE', 'GIMMEH',
    'MAEK', 'IS', 'NOW',
    'NUMBAR', 'YARN', 'TROOF', 'NUMBR',
    'WTF', 'OMG', 'OMGWTF', 'GTFO',
    'AM', 'IN', 'YR', 'TIL', 'WILE', 'UPPIN', 'NERFIN', 'OUTTA', 'IM', 
)

tokens = keywords + (
     'NEWLINE',
     'ID',
     'VARIABLE',
     'INT', 'FLOAT', 'STRING',
     'MINUS',
     'COMMENT', 'MULTILINE_COMMENT',
     'LABEL',
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

def t_LABEL(t):
    r'\w+'
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

loops = [] # a stack of loops
break_loop = False

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
    '''construct : construct NEWLINE construct
                 | statement empty
                 | empty
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
            p[3] = [p[3]]
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


def p_elifs(p):
    '''
    elifs : elif
          | elifs elif
    '''

    if len(p) == 2 and p[1]:
        p[0] = [p[1]]
    elif len(p) == 3:
        p[0] = p[1] + [p[2]]


def p_elif(p):
    '''
    elif : MEBBE expression NEWLINE construct
    '''
    p[0] = (p.lineno(1), 'ELIF', p[2], p[4])


def p_construct_case(p):
    'construct : expression NEWLINE WTF NEWLINE omgs NEWLINE omgwtf OIC '
    p[0] = (p.lineno(2), 'CASE', p[1], p[5], p[7])

def p_construct_omgs(p):
    '''omgs : omg
            | omgs omg
    '''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        if isinstance(p[2], tuple):
            p[2] = [p[2]]
        p[0] = p[1] + (p[2] or [])

def p_construct_omg(p):
    '''omg : OMG literal NEWLINE construct
           | OMG literal NEWLINE construct GTFO 
           | empty
    '''
    if len(p) == 2:
        p[0] = None
    else:
        p[0] = (p.lineno(1), 'OMG', p[2], p[4], p[5] if len(p) >= 6 else None)

def p_construct_omgwtf(p):
    '''omgwtf : empty
              | OMGWTF NEWLINE construct
    '''
    if len(p) == 2:
        p[0] = None
    else:
        p[0] = (p.lineno(1), 'OMGWTF', p[3]) 

def p_loop(p):
    '''construct : IM IN YR VARIABLE operation YR VARIABLE loop_condition NEWLINE construct IM OUTTA YR VARIABLE
                 | IM IN YR VARIABLE operation YR VARIABLE NEWLINE construct IM OUTTA YR VARIABLE'''
    if len(p) == 14:
        p[0] = (p.lineno(1), 'LOOP', p[4], p[5], p[7], None, p[9])
    else:
        p[0] = (p.lineno(1), 'LOOP', p[4], p[5], p[7], p[8], p[10])

def p_loop_condition(p):
    '''loop_condition : TIL expression
                      | WILE expression'''
    if len(p) == 2:
        p[0] = None
    else:
        p[0] = (p[1], p[2])

def p_statement_comment(p):
    'statement : COMMENT NEWLINE'
    p[0] = None

def p_statement_multiline_comment(p):
    'statement : MULTILINE_COMMENT NEWLINE'
    p[0] = None

def p_statement(p):
    'statement : command NEWLINE'
    p[0] = p[1]

def p_command_gtfo(p):
    'command : GTFO'
    p[0] = (p.lineno(1), 'GTFO')

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

def p_command_gimmeh(p):
    'command : GIMMEH VARIABLE'
    p[0] = (p.lineno(1), 'GIMMEH', p[2])


def p_command_cast_var(p):
    'command : VARIABLE IS NOW A type'
    p[0] = (p.lineno(1), 'ASSIGN', p[1], ( p.lineno(1), 'CAST', ( p.lineno(1), 'VAR', p[1] ), p[5]))

def p_expression_cast(p):
    'expression : MAEK expression A type'
    p[0] = (p.lineno(1), 'CAST', p[2], p[4])

def p_expression_binop_both_same(p):
    ''' expression : BOTH SAEM expression expression
                   | BOTH SAEM expression AN expression
    '''
    if len(p) == 5:
        p[0] = (p.lineno(1), 'BINOP', 'BOTH SAEM', p[3], p[4])
    elif len(p) == 6:
        p[0] = (p.lineno(1), 'BINOP', 'BOTH SAEM', p[3], p[5])

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


def p_expression_type(p):
    'expression : type'
    p[0] = p[1]


def p_expression_literal(p):
    'expression : literal'
    p[0] = p[1]

def p_type(p):
    '''type : YARN
          | NUMBR
          | NUMBAR
          | TROOF
          | NOOB
    '''
    p[0] = p[1]

def p_literal_int(p):
    'literal : INT'
    p[0] = (p.lineno(1), 'INT', p[1])

def p_literal_float(p):
    'literal : FLOAT'
    p[0] = (p.lineno(1), 'FLOAT', p[1])

def p_literal_string(p):
    'literal : STRING'
    p[0] = (p.lineno(1), 'STRING', p[1])

def p_literal_true(p):
    'literal : WIN'
    p[0] = (p.lineno(1), 'WIN', True)

def p_literal_false(p):
    'literal : FAIL'
    p[0] = (p.lineno(1), 'FAIL', False)

def p_operation(p):
    '''operation : UPPIN
                 | NERFIN
    '''
    p[0] = p[1]

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
    return '"{}"'.format(text)

def cast(item, to_type):
    if to_type == 'TROOF':
        return bool(item)
    elif to_type == 'NUMBR':
        return int(item)
    elif to_type == 'NUMBAR':
        return float(item)
    elif to_type == 'YARN':
        return str(item)
    elif to_type == 'NOOB':
        return None

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
        'UPPIN': lambda x: x+1,
        'NERFIN': lambda x: x-1,
    }
    return ops[op](x)

def eval_construct(construct):
    for st in sorted(construct, key=lambda st: st[0]):
        if break_loop:
            break
        if st[0] > cur_line:
            eval(st)

def eval(p):
    global cur_line
    global break_loop
    if not p:
        return
    if not isinstance(p, tuple):
        return p
    lineno = p[0]
    if lineno < cur_line:
        raise(Exception('What? You are at line ' + str(cur_line)))
    op = p[1]
    if op in ['INT', 'FLOAT', 'STRING', 'WIN', 'FAIL']:
        return p[2]
    elif op == 'VAR':
        return variables[eval(p[2])]
    elif op in ['DECLARE', 'ASSIGN']:
        variables[eval(p[2])] = eval(p[3])
    elif op == 'VISIBLE':
        text = eval(p[2])
        if isinstance(text, str):
            text = format_lolcode_string(text)
        else:
            text = to_lolcode_type(text)
        print(text)
    elif op == 'GIMMEH':
        varname = eval(p[2])
        val = input('Please input value for variable {} '.format(varname))
        if not varname in variables:
            raise(Exception('Undefined variable ' + varname))
        variables[varname] = val
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
        elifs = p[4] or []
        else_construct = p[5]
        if expr and if_true_construct:
            eval_construct(if_true_construct)
        else:
            for elif_block in elifs:
                if eval(elif_block[2]):
                    eval_construct(elif_block[3])
                    break
            else:
                eval_construct(else_construct)
    elif op == 'CASE':
        def _execute_blocks_until_break(i, omg_blocks):
            block = omg_blocks[i]
            construct, terminated =  block[3], block[4]
            if construct:
                eval_construct(construct)
            if terminated or len(omg_blocks) < i+1:
                return i
            else:
                next_block = omg_blocks[i+1]
                return _execute_blocks_until_break(i+1, omg_blocks)

        expr = eval(p[2])
        omg_blocks = p[3]
        omgwtf = p[4]
        i = 0
        match = False
        while i < len(omg_blocks):
            block = omg_blocks[i]
            literal = eval(block[2])
            if expr == literal:
                match = True
                i = _execute_blocks_until_break(i, omg_blocks)
            i += 1
        if not match and omgwtf:
            construct = omgwtf[2]
            eval_construct(construct) 
    elif op == 'LOOP':
        def resolve_condition(condition):
            # return 0 if break, 1 if continued
            expression = condition[1]
            res = eval(expression)
            if (condition[0] == 'TIL' and res) or (condition[0] == 'WILE' and not res):
                return 0
            return 1

        label = p[2]
        operation = p[3]
        temp_var_name = p[4]
        condition = p[5]
        construct = p[6]
        variables[temp_var_name] = 0
        i = 0
        max_iters = 10**3
        loops.append(label)
        while (resolve_condition(condition) if condition else 1):
            variables[temp_var_name] = unary_op(operation, variables[temp_var_name])
            if construct:
                eval_construct(construct)
            if break_loop: # if GTFO was in construct
                break
            cur_line = p[0] 
            i+=1
            if i >= max_iters:
                raise(Exception('Infinite loop'))
        break_loop = False
        loops.remove(label)
        del variables[temp_var_name]
    elif op == 'GTFO':
        if not loops:
            raise(Exception('GTFO outside of loop'))
        break_loop = True
    elif op == 'CAST':
        expr = eval(p[2])
        to_type = eval(p[3])
        return cast(expr, to_type)
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
