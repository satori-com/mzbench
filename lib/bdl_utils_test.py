#!/usr/bin/env nosetests
# This is nose based tests for benchDL translator, run "pip install nose" if you don't have "nosetests"

import bdl_utils
from nose.tools import eq_

def test_indents():
    eq_(bdl_utils.add_indents('#!benchDL\nmake_install(git = "git@github.com:foo/bar", branch = "b")'),
        '#!benchDL\nmake_install(git = "git@github.com:foo/bar", branch = "b")')

def test_indents_2():
    eq_(bdl_utils.add_indents('#!benchDL\npool(size = 1)\n do_stuff(1,2)'),
        '#!benchDL\npool(size = 1)\n_INDENT_ do_stuff(1,2)\n_DEDENT_ ')

def test_indents_3():
    eq_(bdl_utils.add_indents('#!benchDL\npool(size = 1)\n do_stuff(2,3)\n   #comment\n do_stuff(1,2)'),
        '#!benchDL\npool(size = 1)\n_INDENT_ do_stuff(2,3)\n   #comment\n do_stuff(1,2)\n_DEDENT_ ')

def test_indents_4():
    eq_(bdl_utils.add_indents("""#!benchDL
pool(size = 1,
    worker_type = dummy_worker)
 do_stuff(1,2)"""),
"""#!benchDL
pool(size = 1,
    worker_type = dummy_worker)
_INDENT_ do_stuff(1,2)
_DEDENT_ """)

def test_indents_5():
    eq_(bdl_utils.add_indents("""#!benchDL
        # (
pool(size = 1,
    worker_type = dummy_worker)
 do_stuff(1,2)"""),
"""#!benchDL
        # (
pool(size = 1,
    worker_type = dummy_worker)
_INDENT_ do_stuff(1,2)
_DEDENT_ """)

def test_indents_6():
    eq_(bdl_utils.add_indents('#!benchDL\n'
        'pool(size = 1, attrib = "\\\"#(")\n'
        ' do_stuff(3)\n'
        '   #comment\n do_stuff(1,2)'),
        '#!benchDL\n'
        'pool(size = 1, attrib = "\\\"#(")\n'
        '_INDENT_ do_stuff(3)\n'
        '   #comment\n'
        ' do_stuff(1,2)\n'
        '_DEDENT_ ')

def test_includes():
    eq_(bdl_utils.get_includes(bdl_utils.convert("""#!benchDL
include_resource(test_json, "file.json", json)
pool(size = 17,
    worker_type = dummy_worker)
 do_stuff(1,2)""", {})), [["test_json", "file.json"]])

def test_includes_2():
    eq_(bdl_utils.get_includes(bdl_utils.convert("""#!benchDL
include_resourse()
pool(size = 17,
    worker_type = dummy_worker)
 do_stuff(1,2)""", {})), [])

def test_includes_3():
    eq_(bdl_utils.get_includes(bdl_utils.convert("""#!benchDL
# second comment
include_resource(test_json, "file.json", json)
pool(size = 17,
    worker_type = dummy_worker)
 do_stuff(1,2)""", {})), [["test_json", "file.json"]])

def test_num_of_workers():
    eq_(bdl_utils.get_num_of_workers(bdl_utils.convert("""#!benchDL
pool(size = 17,
    worker_type = dummy_worker)
 do_stuff(1,2)

pool(size = 13,
    worker_type = dummy_worker)
 do_stuff(1,2)""", {})), 30)


def test_num_of_workers_2():
    eq_(bdl_utils.get_num_of_workers(bdl_utils.convert("""#!benchDL
pool(size = var("num2", 3),
    worker_type = dummy_worker)
 do_stuff(1,2)

pool(size = var("num", 3),
    worker_type = dummy_worker)
 do_stuff(1,2)""", {"num": 7})), 10)


def test_num_of_workers_3():
    eq_(bdl_utils.get_num_of_workers(bdl_utils.convert("""#!benchDL
pool(size = 17K,
    worker_type = dummy_worker)
 do_stuff(1,2)

pool(size = 13M,
    worker_type = dummy_worker)
 do_stuff(1,2)""", {})), 13017000)
