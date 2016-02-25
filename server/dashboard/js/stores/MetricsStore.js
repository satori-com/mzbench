import moment from 'moment';
import { EventEmitter } from 'events';
import Dispatcher from '../dispatcher/AppDispatcher';
import ActionTypes from '../constants/ActionTypes';
import MZBenchActions from '../actions/MZBenchActions';
import BenchStore from '../stores/BenchStore';

const CHANGE_EVENT = 'metrics_change';

let data = {
    streams: new Map([])
};

function _updateData(streamId, rawData) {
    const updates = rawData.split("\n");
    updates.forEach((update) => {
        _applyUpdate(streamId, update);
    });
}

function _updateBatchCounter(streamId) {
    data.streams.get(streamId).batch_counter = data.streams.get(streamId).batch_counter + 1;
}

function _applyUpdate(streamId, update) {
    const tokens = update.split("\t");

    if(tokens.length >= 4) {
        const date = Number.parseInt(tokens[0]);
        const value = Number.parseFloat(tokens[1]);
        const min = Number.parseFloat(tokens[2]);
        const max = Number.parseFloat(tokens[3]);

        if(!Number.isNaN(date) && !Number.isNaN(value) && !Number.isNaN(min) && !Number.isNaN(max)) {
            _addObservation(streamId, { date: date, value: value, min: min, max: max });
        }
    }
}

function _addObservation(streamId, observation) {
    data.streams.get(streamId).data.push({
        "date": _convertDate(streamId, observation.date), 
        "value": observation.value, 
        "min": observation.min, 
        "max": observation.max
    });
}

function _convertDate(streamId, rawDate) {
    return rawDate - data.streams.get(streamId).starting_date;
}

function _garbadgeCollectOldData(streamId) {
    const time_window = data.streams.get(streamId).time_window;
    
    if(time_window && data.streams.get(streamId).data.length > 0) {
        const begin_date = data.streams.get(streamId).data[data.streams.get(streamId).data.length - 1].date - time_window;
        
        data.streams.get(streamId).data = data.streams.get(streamId).data.filter((value) => {
            return value["date"] >= begin_date;
        });
    }
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

    updateMetricData(streamId, rawData) {
        if(data.streams.has(streamId)) {
            _updateData(streamId, rawData);
        }
    }

    updateMetricBatchCounter(streamId) {
        if(data.streams.has(streamId)) {
            _updateBatchCounter(streamId);
            _garbadgeCollectOldData(streamId);
        }
    }

    subscribeToEntireMetric(benchId, metric, subsamplingInterval, continue_streaming_after_end) {
        const streamId = MZBenchActions.startStream(benchId, metric, subsamplingInterval, undefined, continue_streaming_after_end);
        data.streams.set(streamId, {
            starting_date: moment(BenchStore.findById(benchId).start_time).unix(),
            time_window: undefined,
            batch_counter: 0,
            data: []
        });
        return streamId;
    }

    subscribeToMetricWithTimeWindow(benchId, metric, timeInterval) {
        const streamId = MZBenchActions.startStream(benchId, metric, 0, timeInterval, true);
        data.streams.set(streamId, {
            starting_date: moment(BenchStore.findById(benchId).start_time).unix(),
            time_window: timeInterval,
            batch_counter: 0,
            data: []
        });
        return streamId;
    }

    unsubscribeFromMetric(streamId) {
        MZBenchActions.stopStream(streamId);
        data.streams.delete(streamId);
    }

    getMetricData(streamId) {
        if(data.streams.has(streamId)) {
            return data.streams.get(streamId).data;
        } else {
            return undefined;
        }
    }

    getBatchCounter(streamId) {
        if(data.streams.has(streamId)) {
            return data.streams.get(streamId).batch_counter;
        } else {
            return undefined;
        }
    }

    getMetricMaxDate(streamId) {
        if(data.streams.has(streamId)) {
            const m = data.streams.get(streamId).data;
            
            if(m.length > 0) {
                return m[m.length - 1]["date"];
            } else {
                return 0;
            }
        } else {
            return undefined;
        }
    }
};

var _MetricsStore = new MetricsStore();
export default _MetricsStore;

_MetricsStore.dispatchToken = Dispatcher.register((action) => {
    switch(action.type) {
        case ActionTypes.METRIC_DATA:
            _MetricsStore.updateMetricData(action.stream_id, action.data);
            _MetricsStore.emitChange();
            break;
        case ActionTypes.METRIC_BATCH_END:
            _MetricsStore.updateMetricBatchCounter(action.stream_id);
            _MetricsStore.emitChange();
            break;
        default:
    }
});
