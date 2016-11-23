var BenchChecker = require('./es5Checker.js');
var IndentAdder = require('indent-adder');
var fs = require("fs");

function checkDir(path) {
    var data = fs.readdirSync(path);
    for (var i = 0; i<data.length; i++) {
        if (fs.lstatSync(path+data[i]).isDirectory()) continue;
        if (data[i].indexOf(".bdl") === -1) continue;

        console.log(path+data[i]);

        var script = fs.readFileSync(path+data[i], 'utf8');

        try {
            var script_id = IndentAdder.add_indents(script, "_INDENT_", "_DEDENT_ ",
                                "#", "'\"", "([", ")]");
            BenchChecker.default.parser.parse(script_id);
        } catch (e) {
            if (e.name  && e.name === "SyntaxError") {
                console.log("Parse error: " + e.message + " Line:" + e.location.start.line + " Column:" + e.location.start.column);
            } else {
                console.log("Parse error: " + e);
            }
            process.exit(1);
        }
    }
}

checkDir("../../examples.bdl/");
checkDir("../../acceptance_tests/scripts.bdl/");