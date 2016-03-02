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

    selectGraph(groupId, graphId) {
        Dispatcher.dispatch({ type: Constants.SELECT_GRAPH, data: {graphData: {groupId: groupId, graphId: graphId}}});
    },

    deselectGraph() {
        Dispatcher.dispatch({ type: Constants.DESELECT_GRAPH });
    },

    cloneBench(id) {
        Dispatcher.dispatch({ type: Constants.CLONE_BENCH, data: id });
    },

    newBench() {
        Dispatcher.dispatch({ type: Constants.NEW_BENCH });
    },

    startStream(benchId, metric, subsamplingInterval, timeWindow, beginTime, endTime, continueStreaming) {
        const streamId = Misc.gen_guid();
        
        let encodedSubsamplingInterval = "undefined";
        if(subsamplingInterval) {
            encodedSubsamplingInterval = subsamplingInterval;
        }
        
        let encodedTimeWindow = "undefined";
        if(timeWindow) {
            encodedTimeWindow = timeWindow;
        }
        
        let encodedBeginTime = "undefined";
        if(beginTime) {
            encodedBeginTime = beginTime;
        }
        
        let encodedEndTime = "undefined";
        if(endTime) {
            encodedEndTime = endTime;
        }
        
        let encodedContinueStreaming = "false";
        if(continueStreaming) {
            encodedContinueStreaming = "true";
        }
        
        MZBenchWS.send({ 
            cmd: "start_streaming_metric", 
            stream_id: streamId, 
            bench: benchId, 
            metric: metric, 
            subsampling_interval: encodedSubsamplingInterval,
            time_window: encodedTimeWindow,
            begin_time: encodedBeginTime,
            end_time: encodedEndTime,
            stream_after_eof: encodedContinueStreaming
        });
        return streamId;
    },
    
    stopStream(streamId) {
        MZBenchWS.send({ cmd: "stop_streaming_metric", stream_id: streamId });
    }
}
