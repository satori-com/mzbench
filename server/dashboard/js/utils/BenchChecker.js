import PegJS from "pegjs";
import IndentAdder from "indent-adder"

const ListFunction = `
{
    function makeList(initial, tail, num) {
        for (var i = 0; i < tail.length; i++) {
            initial.push(tail[i][num]);
        }
        return initial;
    }
    function makeTree(head, tail) {
        var lastop = head;
        for (var i = 0; i < tail.length; i++) {
            lastop = {name: tail[i][1], args: [lastop, tail[i][3]]};
        }
        return lastop;
    }
}`;

const SharedGrammar = `
list
    = _ "[" _ head:term tail:(_ "," _ term)* _ "]" { return makeList([head], tail, 3); }
    / _ "[" _ "]" {return [];}

atom
    = first:[a-z] letters:[0-9a-zA-Z_]* { return first + letters.join(""); }
    / "'" letters:[^']* "'" { return letters.join(""); }

string
    = '"' chars:DoubleStringCharacter* '"' { return chars.join(''); }

DoubleStringCharacter
    = !('"' / "\\\\") char:. { return char; }
    / "\\\\" sequence:EscapeSequence { return sequence; }

EscapeSequence
    = "'"
    / '"'
    / "\\\\"
    / "b"  { return "\\b";   }
    / "f"  { return "\\f";   }
    / "n"  { return "\\n";   }
    / "r"  { return "\\r";   }
    / "t"  { return "\\t";   }
    / "v"  { return "\\x0B"; }

`;

const BdlGrammar = ListFunction + `
entry
    = _ st:(statement _)* _ { return makeList([], st, 0); }

statement
    = multiline / single

multiline
    = name:atom _ args:args _ ":" _ "_INDENT_" _ body:(statement _)+ "_DEDENT_" {
        return {name:name, args:args, body:makeList([], body, 0)};
    }

single
    = name:atom _ args:args {return {name:name, args:args};}

args
    = _ m:map {return m;}
    / _ "(" _ head:term tail:(_ "," _ term)* _ ")" { return makeList([head], tail, 3); }
    / _ "(" _ ")" {return null;}

map
    = "(" _ head:kv tail:(_ "," _ kv)* _ ")" { return makeList([head], tail, 3); }

kv
    = k:term _ "=" _ v:term _ {return {key:k, value:v};}

term
    = unumber / logic_exp / single / list / string / atom / number

logic_exp
    = "(" _ head:logic_exp ")" tail:(_ logic_binary _ logic_exp)* {return makeTree(head, tail);}
    / name:logic_unary _ arg:logic_exp {return {name: name, args: arg};}
    / head:logic_op tail:(_ logic_binary _ logic_exp)* {return makeTree(head, tail);}

logic_binary
    = "and" / "or"

logic_unary
    = "not"

logic_op
    = (string / number) _ ("<=" / ">=" / "<" / ">" / "==") _ (string / number)

number
    = digits:[0-9]+ after:("." [0-9]+)? exp:("e" "-"? [0-9]+)? mult:[GKM]? {
        var base = parseFloat(mult ? text().substring(0, text().length - 1) : text());
        if (mult === "G") base *= 1000000000;
        if (mult === "K") base *= 1000;
        if (mult === "M") base *= 1000000;
        return base;
    }

unumber
    = v:(number / single) _ u:atom {return {value:v, units:u};}

_
    = [\\t\\n\\r ]* ("#" (!"\\n" .)* "\\n" [\\t\\n\\r ]* )*
` + SharedGrammar;

const ErlGrammar = ListFunction + `

entry
    = tr:(term _ "." _)* _ { return makeList([], tr, 0); }

term
    = boolean / atom / list / tuple / map / string / binary / number

tuple
    = _ "{" _ head:term tail:(_ "," _ term)* _ "}" { return makeList(["tuple", head], tail, 3); }
    / _ "{" _ "}" {return [];}

map
    = ( _ "#{" _ head:keyvalue (_ "," _ tail:keyvalue)* _ "}" ) { return makeList([head], tail, 3); }
    / ( _ "#{" _ "}") {return [];}

keyvalue
    = key:term _ "=>" _ value:term _ { return {key:k, value:v}; }

binary
    = "<<" s:string ">>" { return s; }

boolean
    = "true" / "false"

number
    = sign:"-"? digits:[0-9]+ "#" base:[0-9a-zA-Z]+ { return parseInt((sign ? sign : "") + digits.join(""), parseInt(base.join(""))); }
    / sign:"-"? digits:[0-9]+ floating:("." [0-9]+)? exp:(("e" / "E")("-" / "+")? [0-9]+)? { return parseFloat(text()); }

_
    = [\\t\\n\\r ]* ("%" (!"\\n" .)* "\\n" [\\t\\n\\r ]* )*
` + SharedGrammar;

class BenchChecker {
    constructor() {
        this.bdl_parser = PegJS.generate(BdlGrammar);
        this.erl_parser = PegJS.generate(ErlGrammar);
    }

    check_env(b) {
        let envs = {};
        let errors = [];

        b.env.map((x) => {if (x.name && (x.name in envs)) envs[x.name]++; else envs[x.name] = 1;});

        Object.keys(envs).map((key, index) => {
            if (envs[key] > 1) {
                errors.push({severity: "warning",
                    text: "Duplicated enviroment variable: " + key});
            }
        });
        return errors;
    }

    add_vars(vars, morevars) {
        return Object.keys(morevars).reduce((a, x) => {
            if (morevars[x] || !a[x]) a[x] = morevars[x];
            return a;
        }, vars)
    }

    get_vars(ast, shadowed) {
        if (Array.isArray(ast)) {
            return ast.reduce((a, x) => this.add_vars(a, this.get_vars(x, shadowed)), {});
        }
        if (ast.name && ast.args && (ast.name === "var" || ast.name === "numvar")) {
            let c = {};
            if (!shadowed[ast.args[0]])
                c[ast.args[0]] = ast.args[1];
            return c;
        }
        if (ast.name && ast.args && ast.name === "defaults") {
            return ast.args.reduce((a, x) => {a[x.key] = x.value; return a;}, {});
        }

        let newshadowed = Object.keys(shadowed).reduce((a, x) => {a[x] = true; return a;}, {});
        if (ast.name && ast.name === "loop") {
            newshadowed = ast.args.reduce((a, x) => {
                if (x.key === "iterator") a[x.value] = true;
                return a;
            });
        }

        return ["args", "body", "value"].reduce(
            (a, x) => ast[x] ? this.add_vars(a, this.get_vars(ast[x], newshadowed)) : a ,{});
    }

    analyze(b) {
        let result = {env : b.env, extra: []};
        let ast = null;
        let max = b.env.reduce((a, x) => x.id > a ? x.id : a, 0);

        try {
            ast = this.parse(b.script_body);
        } catch (e) {
            return result; // If script can't be parsed, no further analysis is available
        }

        let vars = this.get_vars(ast, {});

        for (var i in result.env) {
            if (result.env[i].name in vars != (!result.env[i].unused)) {
                result.env[i].unused = !result.env[i].unused;
//                result.env[i].id = ++max;
            }
        }

        let usedVars = result.env.reduce((a, x) => {a[x.name] = true; return a;}, {});
        result.extra = Object.keys(vars).reduce(
            (a, x) => usedVars[x] ? a : a.concat([{name: x, value: vars[x], id: ++max}]), []);

        return result;
    }

    parse(text) {
        if (text.indexOf("#!benchDL") === 0) {
            let script_id = IndentAdder.add_indents(text, "_INDENT_", "_DEDENT_ ",
                                "#", "'\"", "([", ")]");
            return this.bdl_parser.parse(script_id);
        } else {
            return this.markup(this.erl_parser.parse(text));
        }
    }

    markup_kv(list) {
        return list.map((x) => {
            return {key: x[1], value: this.markup(x[2])};
        }, this);
    }

    markup(ast) {
        if (ast[0] === "tuple") {
            if (ast[1] === "pool" || ast[1] === "loop") {
                return {name : ast[1], args: this.markup_kv(ast[2]), body: this.markup(ast[3])};
            }
            if (ast[1] === "defaults") {
                return {name : "defaults", args: this.markup_kv(ast[2])};
            }
            if (ast.length === 3 && (typeof ast[1] === "number" || ast[1][0] === "tuple")) {
                return {name : ast[2], args: this.markup(ast[1])};
            }
            ast.shift();
            var name = ast.shift();
            return {name: name, args: this.markup(ast)};
        }

        if (Array.isArray(ast)) {
            return ast.map(this.markup, this);
        } else {
            return ast;
        }
    }

    check_script(b) {
        let ast = null;
        try {
            ast = this.parse(b.script_body);
        } catch (e) {
            if (e.name  && e.name === "SyntaxError") {
                return [{severity: "danger", text: "Parse error: " + e.message + " Line:" + e.location.start.line + " Column:" + e.location.start.column}];
            } else {
                return [{severity: "danger", text: "Parse error: " + e}];
            }
        }

        if (ast.filter((node) => node.name === "make_install").length === 0) {
            let usedWorkers = {};
            ast.filter((node) => node.name === "pool").map(
                (node) => node.args.filter((n) => n.key === "worker_type").map(
                    (n) => {usedWorkers[n.value] = 1}));
            let usedWorkersList = Object.keys(usedWorkers);

            if (usedWorkersList.length > 0 &&
                (usedWorkersList.length > 1 || usedWorkersList[0] !== "dummy_worker")) {
                return [{severity: "warning", text: "Probably missing make_install for " + usedWorkersList}];
            }
        }

        return [];
    }

    get_errors(b) {
        return this.check_env(b).concat(this.check_script(b));
    }
}

var _BenchChecker = new BenchChecker();
export default _BenchChecker;
