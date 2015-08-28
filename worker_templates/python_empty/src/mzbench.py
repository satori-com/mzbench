import os
import sys

# Exported functions

def notify(metric, value):
    _mzbench_pipe.write("M {{{0}, {1}}}.\n".format(_encode_metric(metric), value))


# Internal functions
def _instruction_end(result):
    _mzbench_pipe.write("T {0}.\n".format(_encode_term(result)))


def _instruction_failed((t, o, st)):
    _mzbench_pipe.write("E {0} {1}.\n".format(t, o))


def _module_funcs(module_name):
    return dir(module_name)

    #_mzbench_pipe.write("F {0}.\n".format(_encode_term(FuncList)))


def _encode_term(term):
    T = type(term)
    if   (list == T): return _encode_list(term)
    elif (tuple == T): return _encode_tuple(term)
    elif (dict == T): return _encode_dict(term)
    elif (int == T): return _encode_num(term)
    elif (float == T): return _encode_num(term)
    elif (str == T): return _encode_str(term)
    elif (unicode == T): return _encode_str(term)
    else: return _encode_str("<unknown python term: {0}>".format(term))


def _encode_list(l):
    return '[' + ', '.join([_encode_term(e) for e in l]) + ']'


def _encode_tuple(l):
    return '{' + ', '.join([_encode_term(e) for e in l]) + '}'


def _encode_dict(d):
    return '#{' + ', '.join([ _encode_term(k) + '=>' + _encode_term(d[k]) for k in d]) + '}'


def _encode_num(n):
    return str(n)


def _encode_str(s):
    return '"{0}"'.format(s)


def _encode_funcs_list(func_list):
    return '[' + ', '.join(['"{0}"'.format(e) for e in func_list]) + ']'


def _encode_metric(metric):
    return '{{"{0}", {1}}}'.format(_encode_string_for_erlang(metric[0]), _encode_string_for_erlang(metric[1]))


# May fail in some complicated cases
def _encode_string_for_erlang(string):
    return string.replace('\\', '\\\\').replace('"', '\"')


# MZBench communication initialization
if 'MZ_PYTHON_WORKER_FIFO_NAME' not in os.environ:
    sys.exit("MZ_PYTHON_WORKER_FIFO_NAME environment variable must be defined!")

_mzbench_pipe = open(os.environ['MZ_PYTHON_WORKER_FIFO_NAME'], 'r+', 0)
