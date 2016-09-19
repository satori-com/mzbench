import moment from 'moment';
import { EventEmitter } from 'events';
import Dispatcher from '../dispatcher/AppDispatcher';
import ActionTypes from '../constants/ActionTypes';
import MZBenchActions from '../actions/MZBenchActions';
import BenchStore from '../stores/BenchStore';
import Misc from '../utils/Misc';

const CHANGE_EVENT = 'metrics_change';

const NORMAL_STREAM = 0;
const AGGREGATED_STREAM = 1;

class _StreamElement {
    constructor(benchId, type, normalizeStart) {
        if (benchId != -1)
            this.startingDate   = moment(BenchStore.findById(benchId).start_time).unix();
        else
            this.startingDate   = normalizeStart ? -1 : 0;

        this.timeWindow     = undefined;
        this.batchCounter   = 0;
        this.data           = [];
        this.aggregators    = [];
        
        if(type === AGGREGATED_STREAM) {
            this.parentStreams = [];
            this.lastIdx = 0;
        }
    }
};

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
    data.streams.get(streamId).batchCounter = data.streams.get(streamId).batchCounter + 1;
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
    if (data.streams.get(streamId).startingDate === -1) {
        data.streams.get(streamId).startingDate = observation.date;
    }
    data.streams.get(streamId).data.push({
        "date": _convertDate(streamId, observation.date), 
        "value": observation.value, 
        "min": observation.min, 
        "max": observation.max
    });
}

function _convertDate(streamId, rawDate) {
    return rawDate - data.streams.get(streamId).startingDate;
}

function _garbadgeCollectOldData(streamId) {
    const timeWindow = data.streams.get(streamId).timeWindow;
    
    if(timeWindow && data.streams.get(streamId).data.length > 0) {
        const beginDate = data.streams.get(streamId).data[data.streams.get(streamId).data.length - 1].date - timeWindow;
        
        data.streams.get(streamId).data = data.streams.get(streamId).data.filter((value) => {
            return value["date"] >= beginDate;
        });
    }
}

function _createAggregatedElement(streamId, i) {
    const parentStreams = data.streams.get(streamId).parentStreams;
    
    let currentParentStream = data.streams.get(parentStreams[0]);
    let elem = {
        "date": currentParentStream.data[i]["date"], 
        "value": currentParentStream.data[i]["value"], 
        "min": currentParentStream.data[i]["min"], 
        "max": currentParentStream.data[i]["max"]
    };
    
    for(let j = 1, l = parentStreams.length; j < l; j++) {
        currentParentStream = data.streams.get(parentStreams[j]);
        
        elem["value"] += currentParentStream.data[i]["value"];
        if(elem["min"] > currentParentStream.data[i]["min"]) elem["min"] = currentParentStream.data[i]["min"];
        if(elem["max"] < currentParentStream.data[i]["max"]) elem["max"] = currentParentStream.data[i]["max"];
    }
    
    elem["value"] = elem["value"]/parentStreams.length;
    data.streams.get(streamId).data.push(elem);
}

function _updateAggregatedStream(streamId) {
    let stream = data.streams.get(streamId);
    let minLength = stream.parentStreams.reduce((acc, parentStreamId) => {
        const parentStream = data.streams.get(parentStreamId);
        if(acc === undefined || parentStream.data.length < acc) return parentStream.data.length;
        else return acc;
    }, undefined);
    
    let updatePerformed = false;
    for(; stream.lastIdx < minLength; stream.lastIdx++) {
        _createAggregatedElement(streamId, stream.lastIdx);
        updatePerformed = true;
    }
    
    stream.batchCounter = stream.parentStreams.reduce((acc, parentStreamId) => {
        const parentStream = data.streams.get(parentStreamId);
        if(acc === undefined || parentStream.batchCounter < acc) return parentStream.batchCounter;
        else return acc;
    }, undefined);
    
    if(updatePerformed) {
        _garbadgeCollectOldData(streamId);
        stream.parentStreams.forEach((parentStreamId) => _garbadgeCollectOldData(parentStreamId));
        stream.lastIdx = stream.data.length - 1;
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
            if(!data.streams.get(streamId).aggregators) _garbadgeCollectOldData(streamId);
            
            data.streams.get(streamId).aggregators.forEach((aggregatedStreamId) => {
                _updateAggregatedStream(aggregatedStreamId);
            });
        }
    }

    subscribeToEntireMetric(benchId, metric, subsamplingInterval, continueStreamingAfterEnd, normalizeStart) {
        const streamId = MZBenchActions.startStream(benchId, metric, subsamplingInterval, undefined, undefined, undefined, continueStreamingAfterEnd);
        let elem = new _StreamElement(normalizeStart ? -1 : benchId, NORMAL_STREAM, normalizeStart);
        data.streams.set(streamId, elem);
        return streamId;
    }

    subscribeToMetricSubset(benchId, metric, subsamplingInterval, beginTime, endTime) {
        const startingDate = moment(BenchStore.findById(benchId).start_time).unix();
        const streamId = MZBenchActions.startStream(benchId, metric, subsamplingInterval, undefined, 
                                                    startingDate + Math.floor(beginTime), Math.ceil(startingDate + endTime), false);
        let elem = new _StreamElement(benchId, NORMAL_STREAM);
        data.streams.set(streamId, elem);
        return streamId;
    }

    subscribeToFinalResults(metric, benchIds, kind, x_env) {
        const streamId = MZBenchActions.getFinals(metric, benchIds, kind, x_env);
        let elem = new _StreamElement(-1, NORMAL_STREAM);
        data.streams.set(streamId, elem);
        return streamId;
    }

    subscribeToMetricWithTimeWindow(benchId, metric, timeInterval) {
        const streamId = MZBenchActions.startStream(benchId, metric, 0, timeInterval, undefined, undefined, true);
        let elem = new _StreamElement(benchId, NORMAL_STREAM);
        elem.timeWindow = timeInterval;
        data.streams.set(streamId, elem);
        return streamId;
    }
    
    createAggregatedStream(benchId, parentStreams, timeInterval) {
        const streamId = Misc.gen_guid();
        let elem = new _StreamElement(benchId, AGGREGATED_STREAM);
        elem.timeWindow = timeInterval;
        elem.parentStreams = parentStreams;
        data.streams.set(streamId, elem);
        
        parentStreams.forEach((parentStreamId) => {
            data.streams.get(parentStreamId).aggregators.push(streamId);
        });
        
        return streamId;
    }

    unsubscribeFromMetric(streamId) {
        MZBenchActions.stopStream(streamId);
        data.streams.delete(streamId);
    }
    
    removeAggregatedStream(streamId) {
        let stream = data.streams.get(streamId);
        stream.parentStreams.forEach((parentStreamId) => {
            let parentStream = data.streams.get(parentStreamId);
            parentStream.aggregators = parentStream.aggregators.filter((aggregatedStreamId) => {
                return aggregatedStreamId != streamId;
            });
        });
        
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
            return data.streams.get(streamId).batchCounter;
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
