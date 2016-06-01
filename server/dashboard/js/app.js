import 'babel-core/polyfill';
import React from 'react';
import ReactDOM from "react-dom";
import MZBenchApp from './components/MZBenchApp.react';
import './utils/MZBenchRouter';

ReactDOM.render(
  <MZBenchApp />,
  document.getElementById('mzbench-container')
);
