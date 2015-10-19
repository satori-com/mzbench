import { EventEmitter } from 'events';
import Dispatcher from '../dispatcher/AppDispatcher';
import ActionTypes from '../constants/ActionTypes';
import MZBenchActions from '../actions/MZBenchActions';

const CHANGE_EVENT = 'metrics_change';

let data = {
    benchId: undefined,
    is_loaded: false,
    starting_date: undefined,
    map: new Map([])
};

function _clearData() {
    data.is_loaded = false;
    data.starting_date = undefined;
    data.map.clear();
}

function _updateData(rawData) {
    const updates = rawData.split("\n");
    updates.forEach((update) => {
        _applyUpdate(update);
    });
}

function _applyUpdate(update) {
    const tokens = update.split("\t");
    
    if(tokens.length >= 3) {
        const date = Number.parseInt(tokens[0]);
        const metric = tokens[1];
        const value = Number.parseFloat(tokens[2]);
        
        if(!Number.isNaN(date) && !Number.isNaN(value)) {
            _addObservation(metric, { date: date, value: value });
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
    
    data.map.set(metric, new Array({"date": _convertDate(observation.date), "value": observation.value}));
}

function _updateMetric(metric, observation) {
    data.map.get(metric).push({"date": _convertDate(observation.date), "value": observation.value});
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
    
    changeCurrentBench(benchId) {
        data.benchId = benchId;
        _clearData();
    }
    
    isDataLoaded() {
        return data.is_loaded;
    }
    
    updateMetricData(benchId, rawData) {
        if(data.benchId == benchId) {
            _updateData(rawData);
        }
    }
    
    metricsReadingIsFinished(benchId) {
        if(data.benchId == benchId) {
            data.is_loaded = true;
        }
    }
    
    getMetricData(metric) {
        if(data.map.has(metric)) {
            return data.map.get(metric);
        } else {
            return [];
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
        case ActionTypes.METRICS_UPDATE:
            _MetricsStore.updateMetricData(action.bench, action.data);
            _MetricsStore.emitChange();
            break;
    
        case ActionTypes.METRICS_READING_IS_FINISHED:
            _MetricsStore.metricsReadingIsFinished(action.bench);
            _MetricsStore.emitChange();
            break;
    
        default:
    }
});
