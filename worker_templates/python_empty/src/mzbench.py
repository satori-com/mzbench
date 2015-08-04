import os
import sys

# Exported functions
def define_metrics(metrics_list):
    _mzbench_pipe.write("D {0}.\n".format(_encode_metrics_list(metrics_list)))


def notify(metric, value):
    _mzbench_pipe.write("M {{{0}, {1}}}.\n".format(_encode_metric(metric), value))


# Internal functions
def _instruction_end():
    _mzbench_pipe.write("T\n")


def _instruction_failed():
    _mzbench_pipe.write("E\n")


def _module_funcs(module_name):
    FuncList = dir(module_name)
    _mzbench_pipe.write("F {0}.\n".format(_encode_funcs_list(FuncList)))


def _encode_funcs_list(func_list):
    return '[' + ', '.join(['"{0}"'.format(e) for e in func_list]) + ']'


def _encode_metric(metric):
    return '{{"{0}", {1}}}'.format(_encode_string_for_erlang(metric[0]), _encode_string_for_erlang(metric[1]))


def _encode_metrics_list(metrics_list):
    if isinstance(metrics_list, list):
        return '[' + ', '.join(map(_encode_metrics_list, metrics_list)) + ']'
    else:
        return _encode_metric(metrics_list)

# May fail in some complicated cases
def _encode_string_for_erlang(string):
    return string.replace('\\', '\\\\').replace('"', '\"')

# MZBench communication initialization
if 'MZ_PYTHON_WORKER_FIFO_NAME' not in os.environ:
    sys.exit("MZ_PYTHON_WORKER_FIFO_NAME environment variable must be defined!")

_mzbench_pipe = open(os.environ['MZ_PYTHON_WORKER_FIFO_NAME'], 'r+', 0)
