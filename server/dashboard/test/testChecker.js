var BenchChecker = require('./es5Checker.js');
var IndentAdder = require('indent-adder');
var fs = require("fs");
var assert = require('assert');

describe('checkParser', function() {
    it('All examples should be parsed successfully', function() {
        checkDirParse("../../examples.bdl/");
    });
    it('All positive acceptance tests should be parsed successfully', function() {
        checkDirParse("../../acceptance_tests/scripts.bdl/");
    });
    it('All old examples should be parsed successfully', function() {
        checkDirParse("../../examples/");
    });
    it('All old positive acceptance tests should be parsed successfully', function() {
        checkDirParse("../../acceptance_tests/scripts/");
    });
    it('Make sure malformed scripts produce errors', function() {
        errors = BenchChecker.default.get_errors({env:[], script_body:
                fs.readFileSync("../../acceptance_tests/invalid_scripts/malformed.bdl", 'utf8')});
        assert.ok(errors.length > 0);
    });
    it('Check for duplicated var warning', function() {
        errors = BenchChecker.default.get_errors({env: [{name:"a1", value: "a", id: 1}, {name: "a1", value: "b", id: 2}], script_body:
                fs.readFileSync("../../acceptance_tests/scripts.bdl/vars_defaults.bdl", 'utf8')});
        assert.deepEqual(errors, [{severity: "warning", text: 'Duplicated enviroment variable: a1'}]);
    });
    it('Check for unknown worker warning', function() {
        errors = BenchChecker.default.get_errors({env: [], script_body:
                fs.readFileSync("../../acceptance_tests/invalid_scripts/unknown_worker.bdl", 'utf8')});
        assert.deepEqual(errors, [{severity: "warning", text: 'Probably missing make_install for unknown_worker'}]);
    });
});

describe('checkAnalyzer', function() {
    it('Check vars from a simple script', function() {
        var analyze = BenchChecker.default.analyze({env: [],
                        script_body: fs.readFileSync("../../acceptance_tests/scripts.bdl/vars_defaults.bdl", 'utf8')});
        assert.deepEqual(analyze, {env: [], extra: [{ name: 'var1', value: 'var1_default_value', id: 1 },
                                                 { name: 'var2', value: 'var2_default_value', id: 2 }]});

        var analyze2 = BenchChecker.default.analyze({env: [],
                         script_body: fs.readFileSync("../../acceptance_tests/scripts/vars_defaults.erl", 'utf8')});
        assert.deepEqual(analyze2, {env: [], extra: [{ name: 'var1', value: 'var1_default_value', id: 1 },
                                                  { name: 'var2', value: 'var2_default_value', id: 2 }]});
    });
    it('Check constants with multipliers', function() {
        var analyze = BenchChecker.default.analyze({env: [],
                        script_body: fs.readFileSync("../../acceptance_tests/scripts.bdl/complex_constants.bdl", 'utf8')});
        assert.deepEqual(analyze, {env: [], extra: [{ name: 'wait_ms', value: 1300, id: 1 },
                                                 { name: 'wait_ms_2', value: 5500000, id: 2 }]});
    });

    it('Check vars from a complex script', function() {
        var an2 = {env: [{name: "mysomething", value: "something", unused: true, id: 1}],
                extra: [{ name: 'pool_size', value: undefined, id: 2 },
                        { name: 'jozin', value: undefined, id: 3 },
                        { name: 'missing', value: 'fallback', id: 4 },
                        { name: 'wait_ms', value: 1, id: 5 },
                        { name: 'wait_ms_undefined', value: 5, id: 6 },
                        { name: 'loop_time', value: 1, id: 7 },
                        { name: 'loop_rate', value: 1, id: 8 }]};
        var analyze = BenchChecker.default.analyze({env: [{name: "mysomething", value: "something", id: 1}],
                        script_body: fs.readFileSync("../../acceptance_tests/scripts.bdl/env.bdl", 'utf8')});
        assert.deepEqual(analyze, an2);

        var analyze_old = BenchChecker.default.analyze({env: [{name: "mysomething", value: "something", id: 1}],
                        script_body: fs.readFileSync("../../acceptance_tests/scripts/env.erl", 'utf8')});
        assert.deepEqual(analyze_old, an2);
    });

});

function checkDirParse(path) {
    var data = fs.readdirSync(path);
    for (var i = 0; i<data.length; i++) {
        if (fs.lstatSync(path+data[i]).isDirectory()) continue;
        if ((data[i].indexOf(".bdl") === -1) && (data[i].indexOf(".erl") === -1))
            continue;

        if (data[i] == "syntax_error.erl") continue;

        console.log(path+data[i]);
        var script = fs.readFileSync(path+data[i], 'utf8');
        errors = BenchChecker.default.get_errors({env: [], script_body: script});

        assert.deepEqual(errors, []);
    }
}
