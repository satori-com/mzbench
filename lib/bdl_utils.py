import re
from parsimonious.grammar import Grammar
import parsimonious.exceptions

class ParseError(Exception):
    pass

def is_bdl_scenario(contents):
    return contents.startswith("#!benchDL\n")

def convert(contents, env):
    ast = transform(lex(contents))
    return substitute(ast, env)

def get_includes(ir):
    if isinstance(ir, list):
        return reduce(lambda a, x: a + get_includes(x), ir, [])
    if isinstance(ir, dict):
        if ir["function"] == "include_resource" and isinstance(ir["args"][1], str):
            return [ir["args"][0:2]]
    return []

def get_num_of_workers(ir):
    if isinstance(ir, list):
        return reduce(lambda a, x: a + get_num_of_workers(x), ir, 0)
    if isinstance(ir, dict):
        if ir["function"] == "pool" and isinstance(ir["args"]["size"], int):
            return ir["args"]["size"]
    return 0

def add_indents(text):
    brackets = []
    indents = [0]
    result = []
    comma = None
    for line in text.splitlines():
        old_len = len(brackets)

        tline = line
        while len(tline) > 0:
            if tline.startswith("\\\\"):
                tline = tline[2:]
                continue
            if tline.startswith("\\\""):
                tline = tline[2:]
                continue

            char = tline[0]
            tline = tline[1:]

            if comma != None and char == comma:
                comma = None
            elif char in ['"', "'"]:
                comma = char

            if comma == None:
                if (char == "]" and brackets[-1] == "[") or (char == ")" and brackets[-1] == "("):
                    brackets.pop()
                if char == "(" or char == "[":
                    brackets.append(char)
                if char == "#":
                    break

        if old_len == 0:
            i = get_indent(line)
            if (i == -1) or (i == indents[-1]):
                result.append(line)
                continue
            if i > indents[-1]:
                result.append("_INDENT_" + line)
                indents.append(i)
                continue
            while i < indents[-1]:
                indents.pop()
                line = "_DEDENT_ " + line
            if i != indents[-1]:
                raise ParseError("Incorrect indentation")

        result.append(line)

    line = ""
    while 0 < indents[-1]:
        indents.pop()
        line = "_DEDENT_ " + line

    if line != "":
        result.append(line)

    return "\n".join(result)

def get_indent(line):
    spaces = len(re.match(r"\s*", line).group())
    return -1 if (len(line) <= spaces) or (line[spaces] == "#") else spaces

def substitute(ir, env):
    if isinstance(ir, list):
        return map(lambda a: substitute(a, env), ir)
    if isinstance(ir, dict):
        ir = {k: substitute(v, env) for k, v in ir.items()}

        if "function" in ir:
            if ir["function"] in ["var", "numvar"] and ir["args"][0] in env:
                return env[ir["args"][0]]
            elif ir["function"] in ["var", "numvar"] and len(ir["args"]) > 1:
                return ir["args"][1]

    return ir

def transform(ast):
    if ast.expr_name == "number":
        if ast.text[-1] in "KMG":
            num = ast.text[0:-1]
            if ast.text[-1] == "K":
                mult = 1000
            elif ast.text[-1] == "M":
                mult = 1000000
            else:
                mult = 1000000000
        else:
            num = ast.text
            mult = 1
        return [float(num)*mult] if "." in num else [int(num)*mult]
    elif ast.expr_name == "boolean":
        return [ast.text=="true"]
    elif ast.expr_name == "string":
        return [ast.text[1:-1].replace(r'\"', '"').replace(r'\\\\', '\\\\')] # "something"
    elif ast.expr_name == "atom":
        return [ast.text]
    else:
        lis = reduce(lambda a, x: a + transform(x), ast.children, [])
        if ast.expr_name == "list":
            return [lis]
        if ast.expr_name == "multiline":
            return [{"function": lis[0], "args":lis[1], "children": lis[2:]}]
        if ast.expr_name == "single":
            return [{"function": lis[0], "args":lis[1:]}]
        elif ast.expr_name == "map":
            return [dict(lis)]
        elif ast.expr_name in ["tuple", "kv"]:
            if len(lis) == 2:
                return [tuple(lis)]
            else:
                return [(lis[0], lis[1:])]
        return lis

def lex(text):
    grammar = Grammar("""\
    entry = _ (statement _)* _
    statement = multiline / single
    multiline = atom _ args _ ":" _ "_INDENT_" _ (statement _)+ "_DEDENT_"
    single = atom _ args
    atom = ~"[a-z][0-9a-zA-Z_]*" / ("'" ~"[^']*" "'")
    _ = ~"\s*" (~"#[^\\r\\n]*\s*")*
    args = ( _ map ) / ( _ "(" _ term (_ "," _ term)* _ ")" ) / (_ "(" _ ")")
    map = "(" _ kv (_ "," _ kv)* _ ")"
    list = ( _ "[" _ term (_ "," _ term)* _ "]" ) / ( _ "[" _ "]")
    kv = term _ "=" _ term _
    term = unumber / logic_exp / single / list / string / atom / number
    logic_exp = logic_priority / logic_unary / logic_plain
    logic_priority = "(" _ logic_exp _ ")" _ (logic_binary _ logic_exp _)*
    logic_unary = "not" _ logic_exp _
    logic_binary = "and" / "or"
    logic_plain = logic_op _ (logic_binary _ logic_exp _)*
    logic_op = (string / number) _ ("<=" / ">=" / "<" / ">" / "==" / "!=" / "<>" / "/=") _ (string / number)
    string = '"' ~r'(\\\\.|[^\\\\"])*' '"'
    number = ~"[0-9]+(\.[0-9]+)?(e\-?[0-9]+)?[GKM]?"
    unumber = (number / single) _ atom
    """)
    try:
        return grammar.parse(add_indents(text))
    except parsimonious.exceptions.ParseError as e:
        raise ParseError(e)
