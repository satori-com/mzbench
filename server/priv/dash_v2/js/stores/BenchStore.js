import { EventEmitter } from 'events';
import Dispatcher from '../dispatcher/AppDispatcher';
import ActionTypes from '../constants/ActionTypes';

const CHANGE_EVENT = 'change';

let data = {
    benchmarks: [],
    isLoaded: false,
    selectedBenchId: undefined,
    activeTab: undefined
};

class Bench {
    constructor(props) {
        Object.assign(this, props);
    }

    isRunning() {
        switch (this.status) {
            case "complete":
            case "failed":
            case "stopped":
                return false;
        }
        return true;
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
        if (!data.selectedBenchId && (0 < data.benchmarks.length)) {
            data.selectedBenchId = data.benchmarks[0].id;
        }
    }

    getBenchmarks() {
        return data.benchmarks;
    }

    getSelectedBench() {
        if (!this.isLoaded()) {
            return undefined;
        }
        return this.findById(data.selectedBenchId);
    }

    getActiveTab() {
        return data.activeTab;
    }

    isLoaded() {
        return data.isLoaded;
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

        case ActionTypes.BENCH_INIT_DATA:
            _BenchStore.loadAll(action.data);
            _BenchStore.emitChange();
            break;

        case ActionTypes.SELECT_BENCH_BY_ID:
            data.selectedBenchId = action.data;
            _BenchStore.emitChange();
            break;

        case ActionTypes.SELECT_ACTIVE_TAB:
            data.activeTab = action.data;
            _BenchStore.emitChange();
            break;

        default:
    }
});

