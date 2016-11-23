set -e
npm install
npm install babel-cli babel-preset-es2015
node node_modules/babel-cli/bin/babel.js --presets es2015 js/utils/BenchChecker.js > test/es5Checker.js
node test/testChecker.js
