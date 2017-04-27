import BenchStore from '../stores/BenchStore';
import DashboardStore from '../stores/DashboardStore';
import GlobalStore from '../stores/GlobalStore';
import MetricsStore from '../stores/MetricsStore';
import LogsStore from '../stores/LogsStore';
import Constants from '../constants/ActionTypes';
import Dispatcher from '../dispatcher/AppDispatcher';
import MZBenchWS from '../utils/MZBenchWS';
import Misc from '../utils/Misc.js';

export default {
    subscribeBenchTimeline () {
        let notify = undefined;
        MZBenchWS.connect("/ws", {
            onopen: () => {
                this.wsConnected();
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
            onclose: (reason) => {
                this.hideTimelineLoadingMask();
                if (!notify && reason != "normal") {
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

    wsConnected() {
        let opts = {};

        let benchId = BenchStore.getSelectedId();
        if (benchId) { opts.bench_id = benchId; }
        if (GlobalStore.isDashboardModeOn())
            this.getDashboards(opts);
        else
            this.getTimeline(opts);
        this.getServerInfo()
    },

    turnOnDashboardMode(mode) {
        if (!DashboardStore.isLoaded()) {
            this.getDashboards({});
        }
        Dispatcher.dispatch({ type: Constants.TURN_ON_DASHBOARD_MODE});
    },

    turnOffDashboardMode(mode) {
        if (!BenchStore.isLoaded()) {
            this.getTimeline({});
        }
        Dispatcher.dispatch({ type: Constants.TURN_OFF_DASHBOARD_MODE});
    },

    setFilter(query) {
        Dispatcher.dispatch({ type: Constants.SET_FILTER, data: query });
    },

    withNewBench(lambda) {
        Dispatcher.dispatch({ type: Constants.MODIFY_NEW_BENCH, data: lambda });
    },

    withSelectedDashboard(lambda) {
        Dispatcher.dispatch({ type: Constants.MODIFY_SELECTED_DASHBOARD, data: lambda });
    },

    addChartToSelectedDashboard() {
        Dispatcher.dispatch({ type: Constants.ADD_CHART_TO_SELECTED_DASHBOARD });
    },

    hideTimelineLoadingMask() {
        Dispatcher.dispatch({ type: Constants.HIDE_TIMELINE_LOADING_MASK });
    },

    updateLogQuery(data, benchId) {
        Dispatcher.dispatch({ type: Constants.UPDATE_LOG_QUERY_DATA, bench_id: benchId, data: data });
    },

    updateLogQueryKind(data, benchId) {
        Dispatcher.dispatch({ type: Constants.UPDATE_LOG_QUERY_KIND, bench_id: benchId, data: data });
    },

    updateLogQueryErrors(data, benchId) {
        Dispatcher.dispatch({ type: Constants.UPDATE_LOG_QUERY_ERRORS, bench_id: benchId, data: data });
    },

    showTimelineLoadingMask() {
        if (MZBenchWS.isConnected() && BenchStore.isLoaded()) {
            Dispatcher.dispatch({ type: Constants.SHOW_TIMELINE_LOADING_MASK });
        }
    },

    resetNewBench() {
        Dispatcher.dispatch({ type: Constants.CLEAN_NEW_BENCH });
    },

    subscribeBenchset(opts) {
        Object.assign(opts, {cmd: "subscribe_benchset"});
        MZBenchWS.send(opts);
    },

    unsubscribeBenchset(benchsetId) {
        MZBenchWS.send({cmd: "unsubscribe_benchset", benchset_id: benchsetId});
    },

    getTimeline(opts, timelineId) {

        Object.assign(opts, {cmd: "get_timeline"});

        if (timelineId) {
            Object.assign(opts, {timeline_id: timelineId});
        } else {
//            this.showTimelineLoadingMask();
            opts.q = BenchStore.getFilter();
        }

        if (undefined == opts.bench_id) {
            BenchStore.getCurrentPage().forEach((value, key) => opts[key] = value)
        }

        MZBenchWS.send(opts);
    },

    saveSelectedDashboard() {
        if (DashboardStore.isNewSelected()) {
            MZBenchWS.send({cmd: "create_dashboard", data: DashboardStore.getNew()});
            this.getDashboards({});
        } else {
            MZBenchWS.send({cmd: "update_dashboard", data: DashboardStore.getSelected()});
        }
    },

    getDashboards(opts) {
        Object.assign(opts, {cmd: "get_dashboards"});

        opts.q = DashboardStore.getFilter();

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

    selectDashboardById(dashboardId) {
        Dispatcher.dispatch({ type: Constants.SELECT_DASHBOARD_BY_ID, data: dashboardId });
    },

    selectActiveTab(tab) {
        Dispatcher.dispatch({ type: Constants.SELECT_ACTIVE_TAB, data: tab });
    },

    selectDashboardTab(tab) {
        Dispatcher.dispatch({ type: Constants.SELECT_DASHBOARD_ACTIVE_TAB, data: tab });
    },

    selectGraph(groupId, graphId) {
        Dispatcher.dispatch({ type: Constants.SELECT_GRAPH, data: {graphData: {groupId: groupId, graphId: graphId}}});
    },

    deselectGraph() {
        Dispatcher.dispatch({ type: Constants.DESELECT_GRAPH });
    },

    saveToggledGraphs(benchId, toggles) {
        Dispatcher.dispatch({ type: Constants.SAVE_TOGGLED_GRAPHS, data:{benchId: benchId, toggles: toggles}});
    },

    cloneBench(id) {
        Dispatcher.dispatch({ type: Constants.CLONE_BENCH, data: id });
    },

    newBench() {
        Dispatcher.dispatch({ type: Constants.NEW_BENCH });
    },

    newDashboard() {
        Dispatcher.dispatch({ type: Constants.NEW_DASHBOARD });
    },

    updateBenchName(benchId, newName) {
        MZBenchWS.send({ cmd: "update_name", bench: benchId, name: newName });
    },

    addBenchTag(benchId, tag) {
        MZBenchWS.send({ cmd: "add_tag", bench: benchId, tag: tag });
    },

    removeBenchTag(benchId, tag) {
        MZBenchWS.send({ cmd: "remove_tag", bench: benchId, tag: tag });
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

    getFinals(metric, benchIds, kind, x_env) {
        const streamId = Misc.gen_guid();

        MZBenchWS.send({
            cmd: "get_finals",
            stream_id: streamId,
            bench_ids : benchIds,
            metric: metric,
            kind: kind,
            x_env: x_env
        });
        return streamId;
    },

    stopStream(streamId) {
        MZBenchWS.send({ cmd: "stop_streaming_metric", stream_id: streamId });
    },

    startStreamLogs(benchId) {
        const streamId = Misc.gen_guid();
        MZBenchWS.send({ cmd: "start_streaming_logs", bench: benchId, stream_id: streamId });
        return streamId;
    },

    stopStreamLogs(streamId) {
        MZBenchWS.send({ cmd: "stop_streaming_logs", stream_id: streamId });
    }

}
