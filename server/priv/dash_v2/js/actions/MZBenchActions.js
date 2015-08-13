import Dispatcher from '../dispatcher/AppDispatcher';
import Constants from '../constants/ActionTypes.js';
import * as api from '../utils/bench_api_emulator'

import MZBenchWS from '../utils/MZBenchWS';
import BenchStore from '../stores/BenchStore';

export function spawn(fn) {
    setTimeout(fn.bind(this), 0);
}

export default {
    subscribeBenchTimeline () {
        MZBenchWS.connect("/ws", {
            onopen: () => {
                let opts = {};

                let benchId = BenchStore.getSelectedBenchId();
                if (benchId) { opts.bench_id = benchId; }

                this.getTimeline(opts);
            },
            onmessage: (data) => {
                Dispatcher.dispatch(data);
            }
        });
    },

    applyQueryParameters(opts) {
        if (undefined !== opts.q) {
            Dispatcher.dispatch({ type: Constants.SET_FILTER, data: opts.q });
        }

        let currentPage = new Map();
        if (undefined !== opts.max_id) currentPage.set("max_id", opts.max_id);
        if (undefined !== opts.min_id) currentPage.set("min_id", opts.min_id);

        Dispatcher.dispatch({ type: Constants.SET_CURRENT_PAGE, data: currentPage });
    },

    getTimeline(opts) {
        Object.assign(opts, {cmd: "get_timeline"});

        Dispatcher.dispatch({ type: Constants.CLEAN_TIMELINE });

        opts.q = BenchStore.getFilter();

        if (undefined == opts.bench_id) {
            BenchStore.getCurrentPage().forEach((value, key) => opts[key] = value)
        }

        MZBenchWS.send(opts);
    },

    unsubscribeBenchTimeline () {
        MZBenchWS.close();
    },

    selectBenchById (benchId) {
        Dispatcher.dispatch({ type: Constants.SELECT_BENCH_BY_ID, data: benchId });

    },

    selectActiveTab(tab) {
        Dispatcher.dispatch({ type: Constants.SELECT_ACTIVE_TAB, data: tab });
    },

    resetMetrics() {
        Dispatcher.dispatch({ type: Constants.METRIC_STORE_RESET });
    },

    subscribeMetrics(benchId) {
        return api.subscribeMetrics({
            params: {benchId: benchId}, // from
            onData: (data) => {
                Dispatcher.dispatch(data);
            },
            onClose: () => {
                setTimeout(() => subscribeMetrics(benchId), 10000);
            }
        });
    },

    unsubscribeMetrics(ws) {
        api.unsubscribeMetrics(ws);
    }
}
