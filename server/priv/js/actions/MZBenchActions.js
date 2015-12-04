import BenchStore from '../stores/BenchStore';
import MetricsStore from '../stores/MetricsStore';
import Constants from '../constants/ActionTypes';
import Dispatcher from '../dispatcher/AppDispatcher';
import MZBenchWS from '../utils/MZBenchWS';
import Misc from '../utils/Misc.js';

export default {
    subscribeBenchTimeline () {
        let notify = undefined;
        MZBenchWS.connect("/ws", {
            onopen: () => {
                let opts = {};

                let benchId = BenchStore.getSelectedBenchId();
                if (benchId) { opts.bench_id = benchId; }

                this.getTimeline(opts);
                this.getServerInfo();

                if (notify) {
                    notify.update({message: 'The board has connected to the server', type: 'success'});
                    setTimeout(() => {
                            notify.close();
                            notify = undefined;
                        }, 5000);
                }
            },
            onmessage: (data) => {
                Dispatcher.dispatch(data);
            },
            onclose: () => {
                this.hideTimelineLoadingMask();
                if (!notify) {
                    notify = $.notify(
                                    { message: "The board is not connected to the server" },
                                    { type: "danger", delay: 0 }
                                  );
                }
            }
        });
    },

    applyQueryParameters(opts) {
        if (undefined !== opts.q) {
            this.setFilter(opts.q);
        }

        let currentPage = new Map();
        if (undefined !== opts.max_id) currentPage.set("max_id", parseInt(opts.max_id));
        if (undefined !== opts.min_id) currentPage.set("min_id", parseInt(opts.min_id));

        Dispatcher.dispatch({ type: Constants.SET_CURRENT_PAGE, data: currentPage });
    },

    setFilter(query) {
        Dispatcher.dispatch({ type: Constants.SET_FILTER, data: query });
    },

    withNewBench(lambda) {
        Dispatcher.dispatch({ type: Constants.MODIFY_NEW_BENCH, data: lambda });
    },

    hideTimelineLoadingMask() {
        Dispatcher.dispatch({ type: Constants.HIDE_TIMELINE_LOADING_MASK });
    },

    showTimelineLoadingMask() {
        if (MZBenchWS.isConnected() && BenchStore.isLoaded()) {
            Dispatcher.dispatch({ type: Constants.SHOW_TIMELINE_LOADING_MASK });
        }
    },

    resetNewBench() {
        Dispatcher.dispatch({ type: Constants.CLEAN_NEW_BENCH });
    },

    getTimeline(opts) {
        Object.assign(opts, {cmd: "get_timeline"});

        this.showTimelineLoadingMask();

        opts.q = BenchStore.getFilter();

        if (undefined == opts.bench_id) {
            BenchStore.getCurrentPage().forEach((value, key) => opts[key] = value)
        }

        MZBenchWS.send(opts);
    },

    getServerInfo(opts) {
        MZBenchWS.send({cmd: "get_server_info"});
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

    cloneBench(id) {
        Dispatcher.dispatch({ type: Constants.CLONE_BENCH, data: id });
    },

    newBench() {
        Dispatcher.dispatch({ type: Constants.NEW_BENCH });
    },

    sendSubscribe(benchId, metrics, guid) {
        MZBenchWS.send({ cmd: "subscribe_metrics", bench: benchId, metrics: metrics, guid: guid });
    },

    subscribeMetrics(metrics) {
        Dispatcher.dispatch({ type: Constants.SUBSCRIBE_METRICS, metrics: metrics });
    }
}
