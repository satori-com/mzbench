import 'babel-core/polyfill';


import React from 'react';
import { Router } from 'director';

import MZBenchActions from './actions/MZBenchActions'
import MZBenchApp from './components/MZBenchApp.react';


let routes = {
    '/bench/:benchId/:activeTab': (benchId, activeTab) => {
        MZBenchActions.selectBenchById(benchId);
        MZBenchActions.selectActiveTab(activeTab);
    }
};

let router = Router(routes);

router.init();

React.render(
  <MZBenchApp />,
  document.getElementById('mzbench-container')
);
