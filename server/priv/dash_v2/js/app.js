import 'babel-core/polyfill';
import React from 'react';
import MZBenchApp from './components/MZBenchApp.react';
import MZBenchRouter from './utils/MZBenchRouter';

React.render(
  <MZBenchApp />,
  document.getElementById('mzbench-container')
);
