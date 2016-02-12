import moment from 'moment';
import { EventEmitter } from 'events';
import Dispatcher from '../dispatcher/AppDispatcher';
import ActionTypes from '../constants/ActionTypes';

const CHANGE_EVENT = 'bench_change';

const defaultData = {
    benchmarks: [],
    server_date_diff: moment.duration(0),
    filter: "",
    pager: {},
    currentPage: new Map(),
    isLoaded: false,
    isNewSelected: false,
    isNewActive: false,
    clouds: [],
    newBench: {
        benchmark_name: "Something",
        script_name: "generic.erl",
        script_body: "[ % the simplest example\n    {pool, [{size, 3}, % three execution \"threads\"\n"
                        +"            {worker_type, dummy_worker}],\n        [{loop, [{time, {5, min}}, % total loop time\n"
                        +"                 {rate, {1, rps}}], % one rps for every worker, 3 rps totally\n"
                        +"                [{print, \"FOO\"}]}]} % this operation prints \"FOO\" to console\n].",
        nodes: "1",
        cloud: "",
        env: {}},
    selectedBenchId: undefined,
    isShowTimelineLoadingMask: false,
    activeTab: undefined,
    activeGraph: undefined
};

let data = jQuery.extend(true, {}, defaultData); // extend is used to copy object recursively

class Bench {
    constructor(props) {
        Object.assign(this, props);
    }

    isRunning() {
        switch (this.status) {
            case "complete":
            case "failed":
            case "zombie":
            case "stopped":
                return false;
        }
        return true;
    }

    get start_time_client() {
        return moment(this.start_time).add(data.server_date_diff);
    }

    get finish_time_client() {
        if (this.finish_time) {
            return moment(this.finish_time).add(data.server_date_diff);
        } else {
            return undefined;
        }
    }
}

class BenchStore extends EventEmitter {
    emitChange() {
        return this.emit(CHANGE_EVENT);
    }

    onChange(callback) {
        this.on(CHANGE_EVENT, callback);
    }

    off(callback) {
        this.removeListener(CHANGE_EVENT, callback);
    }

    findById(id) {
        return data.benchmarks.find(x => x.id == id);
    }

    updateBench(bench) {
        let existBench = this.findById(bench.id);
        if (existBench) {
            Object.assign(existBench, bench);
        } else {
            data.benchmarks.unshift(new Bench(bench));
        }
    }

    loadAll(benchmarks) {
        benchmarks.sort((a, b) => b.id - a.id);
        data.benchmarks = benchmarks.map((b) => new Bench(b));
        data.isLoaded = true;
        if (!this.getSelectedBench() && (0 < data.benchmarks.length)) {
            data.selectedBenchId = data.benchmarks[0].id;
        }
    }

    getBenchmarks() {
        return data.benchmarks;
    }

    getSelectedBench() {
        if (!this.isLoaded() || this.isNewSelected()) {
            return undefined;
        }
        return this.findById(data.selectedBenchId);
    }

    getSelectedBenchId() {
        return data.selectedBenchId;
    }

    getActiveTab() {
        return data.activeTab;
    }

    getSelectedGraph() {
        return data.activeGraph;
    }

    isLoaded() {
        return data.isLoaded;
    }

    isNewSelected() {
        return data.isNewSelected;
    }

    isNewActive() {
        return data.isNewActive;
    }

    getClouds() {
        return data.clouds;
    }

    getNewBench() {
        return data.newBench;
    }

    resetNewBench() {
        data.newBench = Object.assign({}, defaultData.newBench);
    }

    cloneNewBench(id) {
        data.newBench = Object.assign({}, this.findById(data.selectedBenchId)); 
    }

    isShowTimelineLoadingMask() {
        return data.isShowTimelineLoadingMask;
    }

    getFilter() {
        return data.filter;
    }

    getPager() {
        return data.pager;
    }

    getCurrentPage() {
        return data.currentPage;
    }
}

var _BenchStore = new BenchStore();
export default _BenchStore;

_BenchStore.dispatchToken = Dispatcher.register((action) => {
    switch (action.type) {
        case ActionTypes.UPDATE_BENCH_INFO:
            _BenchStore.updateBench(action.data);
            _BenchStore.emitChange();
            break;

        case ActionTypes.INIT_TIMELINE:
            data.server_date_diff = moment().diff(moment(action.server_date));

            _BenchStore.loadAll(action.data);
            data.pager = action.pager;
            data.isShowTimelineLoadingMask = false;
            if (data.selectedBenchId === undefined) {
                data.isNewSelected = true;
                data.isNewActive = true;
            }
            _BenchStore.emitChange();
            break;

        case ActionTypes.SELECT_BENCH_BY_ID:
            data.isNewSelected = false;
            data.selectedBenchId = parseInt(action.data);
            _BenchStore.emitChange();
            break;

        case ActionTypes.SELECT_ACTIVE_TAB:
            data.activeTab = action.data;
            _BenchStore.emitChange();
            break;

        case ActionTypes.SELECT_GRAPH:
            data.activeGraph = action.data.graphData;
            _BenchStore.emitChange();
            break;

        case ActionTypes.DESELECT_GRAPH:
            data.activeGraph = undefined;
            _BenchStore.emitChange();
            break;

        case ActionTypes.SHOW_TIMELINE_LOADING_MASK:
            data.isShowTimelineLoadingMask = true;
            _BenchStore.emitChange();
            break;

        case ActionTypes.HIDE_TIMELINE_LOADING_MASK:
            data.isShowTimelineLoadingMask = false;
            _BenchStore.emitChange();
            break;

        case ActionTypes.SET_CURRENT_PAGE:
            data.currentPage = action.data;
            break;

        case ActionTypes.SET_FILTER:
            data.filter = action.data;
            _BenchStore.emitChange();
            break;

        case ActionTypes.NEW_BENCH:
            data.selectedBenchId = undefined;
            data.isNewActive = true;
            data.isNewSelected = true;
            _BenchStore.emitChange();
            break;

        case ActionTypes.MODIFY_NEW_BENCH:
            data.isNewActive = true;
            action.data(data.newBench);
            _BenchStore.emitChange();
            break;

        case ActionTypes.CLONE_BENCH:
            data.newBench = Object.assign({}, data.benchmarks.find(x => x.id == action.data));
            data.selectedBenchId = undefined;
            data.isNewSelected = true;
            data.isNewActive = true;
            data.currentPage = new Map();
            _BenchStore.emitChange();
            break;

        case ActionTypes.CLEAN_NEW_BENCH:
            data.newBench = Object.assign({}, defaultData.newBench);
            if (data.clouds.length > 0) {
                data.newBench.cloud = data.clouds[0];
            }
            data.isNewActive = false;
            _BenchStore.emitChange();
            break;

        case ActionTypes.SERVER_INFO:
            data.clouds = action.data.clouds;
            if ((data.clouds.length > 0) && (data.newBench.cloud === ""))
                data.newBench.cloud = data.clouds[0];
            _BenchStore.emitChange();
            break;

        case ActionTypes.NOTIFY:
            $.notify({message: action.message}, {type: action.severity});
            break;

        default:
    }
});

