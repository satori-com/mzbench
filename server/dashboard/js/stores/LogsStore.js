import moment from 'moment';
import { EventEmitter } from 'events';
import Dispatcher from '../dispatcher/AppDispatcher';
import ActionTypes from '../constants/ActionTypes';
import MZBenchActions from '../actions/MZBenchActions';
import BenchStore from '../stores/BenchStore';

const CHANGE_EVENT = 'logs_change';

let data = {
    queries: new Map([]),
    stream: {id:-1}
};

function _performUpdate(arrayRef, rawData) {
    const Reg = /(\d{2}:\d{2}:\d{2}.\d{3})\s(\[[^\]]*\])/
    var parts;
    if (arrayRef.length > 0) {
        parts = (arrayRef[arrayRef.length - 1].text + rawData).split(Reg);
        arrayRef[arrayRef.length - 1].text = parts[0];
    } else parts = rawData.split(Reg);

    for (var i = 1; i + 2 < parts.length; i += 3)
        arrayRef.push({id: arrayRef.length, time: parts[i], severity: parts[i+1], text: parts[i+2]});
}

function _updateData(rawData) {
    _performUpdate(data.stream.system, rawData);
}

function _updateUserData(rawData) {
    _performUpdate(data.stream.user, rawData);
}

function _updateQuery(benchId, newData) {
    data.queries.set(benchId, newData);
}

class LogsStore extends EventEmitter {
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

    updateLogData(streamId, rawData) {
        if(data.stream.id == streamId) {
            _updateData(rawData);
        }
    }

    updateLogUserData(streamId, rawData) {
        if(data.stream.id == streamId) {
            _updateUserData(rawData);
        }
    }

    updateLogOverflow(streamId) {
        if (data.stream.id == streamId)
            data.stream.systemOverflow = 1;
    }

    updateLogUserOverflow(streamId) {
        if (data.stream.id == streamId)
            data.stream.userOverflow = 1;
    }

    subscribeToLogs(benchId) {
        const streamId = MZBenchActions.startStreamLogs(benchId);
        data.stream = {
            id: streamId,
            user: [],
            userOverflow : 0,
            system: [],
            systemOverflow: 0
        }
        return streamId;
    }

    unsubscribeFromLogs(streamId) {
        MZBenchActions.stopStreamLogs(streamId);
        data.stream.id = 0;
    }

    getQueryData(benchId) {
        if(data.queries.has(benchId)) {
            return data.queries.get(benchId);
        } else {
            return {kind:0, errors: 0, query:""};
        }
    }

    updateQueryData(benchId, data) {
        let qdata = this.getQueryData(benchId);
        qdata.query = data;
        _updateQuery(benchId, qdata);
    }

    updateQueryKind(benchId, data) {
        let qdata = this.getQueryData(benchId);
        qdata.kind = data;
        _updateQuery(benchId, qdata);
    }

    updateQueryErrors(benchId, data) {
        let qdata = this.getQueryData(benchId);
        qdata.errors = data;
        _updateQuery(benchId, qdata);
    }

    getLogData(streamId) {
        if (data.stream.id == streamId) {
            return data.stream;
        } else {
            return {user:[], userOverflow: 0, system: [], systemOverflow: 0};
        }
    }
};

var _LogsStore = new LogsStore();
export default _LogsStore;

_LogsStore.dispatchToken = Dispatcher.register((action) => {
    switch(action.type) {
        case ActionTypes.LOG_DATA:
            _LogsStore.updateLogData(action.stream_id, action.data);
            _LogsStore.emitChange();
            break;
        case ActionTypes.LOG_USER_DATA:
            _LogsStore.updateLogUserData(action.stream_id, action.data);
            _LogsStore.emitChange();
            break;
        case ActionTypes.LOG_OVERFLOW:
            _LogsStore.updateLogOverflow(action.stream_id);
            _LogsStore.emitChange();
            break;
        case ActionTypes.LOG_USER_OVERFLOW:
            _LogsStore.updateLogUserOverflow(action.stream_id);
            _LogsStore.emitChange();
            break;
        case ActionTypes.UPDATE_LOG_QUERY_DATA:
            _LogsStore.updateQueryData(action.bench_id, action.data);
            _LogsStore.emitChange();
            break;
        case ActionTypes.UPDATE_LOG_QUERY_KIND:
            _LogsStore.updateQueryKind(action.bench_id, action.data);
            _LogsStore.emitChange();
            break;
        case ActionTypes.UPDATE_LOG_QUERY_ERRORS:
            _LogsStore.updateQueryErrors(action.bench_id, action.data);
            _LogsStore.emitChange();
            break;
        default:
    }
});
