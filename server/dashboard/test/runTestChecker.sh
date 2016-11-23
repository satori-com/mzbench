npm install babel-preset-latest babel-cli
node node_modules/babel-cli/bin/babel.js --presets es2015 js/utils/BenchChecker.js > test/es5Checker.js
node test/testChecker.js
