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


def _module_funcs(module_name):
    FuncList = dir(module_name)
    _mzbench_pipe.write("F {0}.\n".format(_encode_funcs_list(FuncList)))


def _encode_funcs_list(FuncList):
    result = '['
    
    is_first_element = True
    for e in FuncList:
        if is_first_element:
           is_first_element = False 
        else:
            result = result + ', '
        
        result = result + '"{0}"'.format(e)
        
    result = result + ']'
    return result


def _encode_metric(metric):
    return '{{"{0[0]}", {0[1]}}}'.format(metric)


def _encode_metrics_list(metrics_list):
    result = '['
    
    is_first_element = True
    for e in metrics_list:
        if is_first_element:
           is_first_element = False 
        else:
            result = result + ', '
            
        if isinstance(e, list):
            result = result + _encode_metrics_list(e)
        else:
            result = result + _encode_metric(e)
    
    result = result + ']'
    return result


# MZBench communication initialization
if 'MZ_PYTHON_WORKER_FIFO_NAME' not in os.environ:
    sys.exit("MZ_PYTHON_WORKER_FIFO_NAME environment variable must be defined!")

_mzbench_pipe = open(os.environ['MZ_PYTHON_WORKER_FIFO_NAME'], 'r+', 0)
