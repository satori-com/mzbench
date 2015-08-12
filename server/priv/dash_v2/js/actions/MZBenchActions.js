import Dispatcher from '../dispatcher/AppDispatcher';
import Constants from '../constants/ActionTypes.js';
import * as api from '../utils/bench_api_emulator'

export function spawn(fn) {
    setTimeout(fn.bind(this), 0);
}

export default {
    subscribeBenchTimeline () {
        api.subscribeBenchTimeline({
            params: {initDataLimit: 15}, // from
            onData: (data) => {
                Dispatcher.dispatch(data);
            },
            onClose: () => {
                setTimeout(subscribeBenchTimeline.bind(this), 10000);
            }
        });
    },

    unsubscribeBenchTimeline () {
        api.unsubscribeBenchTimeline();
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
