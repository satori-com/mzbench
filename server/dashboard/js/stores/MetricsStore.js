import { EventEmitter } from 'events';
import Dispatcher from '../dispatcher/AppDispatcher';
import ActionTypes from '../constants/ActionTypes';
import MZBenchActions from '../actions/MZBenchActions';
import Misc from '../utils/Misc.js';

const CHANGE_EVENT = 'metrics_change';
const DATA_RESAMPLING_RATE = 1; // hours

let data = {
    benchId: undefined,
    guid: undefined,
    
    last_resampling_date: undefined,
    
    starting_date: undefined,
    map: new Map([]),
    batch_counter: new Map([])
};

function _clearData() {
    data.last_resampling_date = Date.now();
    
    data.starting_date = undefined;
    data.map.clear();
    data.batch_counter.clear();
}

function _updateData(metric, rawData) {
    const updates = rawData.split("\n");
    updates.forEach((update) => {
        _applyUpdate(metric, update);
    });
}

function _updateBatchCounter(metric) {
    let m = data.batch_counter;
    if (m.has(metric)) {
        m.set(metric, m.get(metric) + 1);
    } else {
        m.set(metric, 1);
    }
}

function _applyUpdate(metric, update) {
    const tokens = update.split("\t");

    if(tokens.length >= 4) {
        const date = Number.parseInt(tokens[0]);
        const value = Number.parseFloat(tokens[1]);
        const min = Number.parseFloat(tokens[2]);
        const max = Number.parseFloat(tokens[3]);

        if(!Number.isNaN(date) && !Number.isNaN(value) && !Number.isNaN(min) && !Number.isNaN(max)) {
            _addObservation(metric, { date: date, value: value, min: min, max: max });
        }
    }
}

function _addObservation(metric, observation) {
    if(data.map.has(metric)) {
        _updateMetric(metric, observation);
    } else {
        _createMetric(metric, observation);
    }
}

function _createMetric(metric, observation) {
    if(!data.starting_date) {
        data.starting_date = observation.date;
    }

    data.map.set(metric, new Array({"date": _convertDate(observation.date), "value": observation.value, "min": observation.min, "max": observation.max}));
}

function _updateMetric(metric, observation) {
    data.map.get(metric).push({"date": _convertDate(observation.date), "value": observation.value, "min": observation.min, "max": observation.max});
}

function _convertDate(rawDate) {
    return rawDate - data.starting_date;
}

class MetricsStore extends EventEmitter {
    constructor() {
        super();
        this.setMaxListeners(Infinity);
    }

    emitChange() {
        return this.emit(CHANGE_EVENT);
    }

    onChange(callback) {
        this.on(CHANGE_EVENT, callback);
    }

    off(callback) {
        this.removeListener(CHANGE_EVENT, callback);
    }

    getCurrentBenchId() {
        return data.benchId;
    }

    performResamplingIfNeeded() {
        const sec_from_last_resampling = (Date.now() - data.last_resampling_date)/1000;
        if(sec_from_last_resampling > DATA_RESAMPLING_RATE*3600) {
            let metrics_list = [];
            data.map.forEach((value, key) => {
                metrics_list.push(key);
            });
        
            this.resetSubscriptions(data.benchId);
            metrics_list.map((metric) => {
                this.addSubscription(metric);
            });
        }
    }

    resetSubscriptions(newBenchId) {
        _clearData();
        data.benchId = newBenchId;
        data.guid = Misc.gen_guid();
    }

    addSubscription(metrics) {
        MZBenchActions.sendSubscribe(data.benchId, metrics, data.guid);
    }

    changeCurrentBench(benchId, GUID) {
        data.benchId = benchId;
        data.guid = GUID;
        _clearData();
    }

    isDataLoaded() {
        return true;
    }

    updateMetricData(metric, guid, rawData) {
        if(data.guid == guid) {
            _updateData(metric, rawData);
        }
    }

    updateMetricBatchCounter(metric, guid) {
        if(data.guid == guid) {
            _updateBatchCounter(metric);
            this.performResamplingIfNeeded();
        }
    }

    getMetricData(metric) {
        if(data.map.has(metric)) {
            return data.map.get(metric);
        } else {
            return [];
        }
    }

    getBatchCounter(metric) {
        if(data.batch_counter.has(metric)) {
            return data.batch_counter.get(metric);
        } else {
            return 0;
        }
    }

    getMetricMaxDate(metric) {
        if(data.map.has(metric)) {
            let m = data.map.get(metric);
            return m[m.length - 1]["date"];
        } else {
            return 0;
        }
    }
};

var _MetricsStore = new MetricsStore();
export default _MetricsStore;

_MetricsStore.dispatchToken = Dispatcher.register((action) => {
    switch(action.type) {
        case ActionTypes.SUBSCRIBE_METRICS:
            _MetricsStore.addSubscription(action.metrics);
            break;
        case ActionTypes.METRIC_DATA:
            _MetricsStore.updateMetricData(action.metric, action.guid, action.data);
            _MetricsStore.emitChange();
            break;
        case ActionTypes.METRIC_BATCH_END:
            _MetricsStore.updateMetricBatchCounter(action.metric, action.guid);
            _MetricsStore.emitChange();
            break;
        default:
    }
});
