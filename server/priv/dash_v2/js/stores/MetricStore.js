import { EventEmitter } from 'events';
import Dispatcher from '../dispatcher/AppDispatcher';
import ActionTypes from '../constants/ActionTypes';

const CHANGE_EVENT = 'change';

let metrics = {};

class MetricStore extends EventEmitter {
    emitChange() {
        return this.emit(CHANGE_EVENT);
    }

    onChange(callback) {
        this.on(CHANGE_EVENT, callback);
    }

    off(callback) {
        this.removeListener(CHANGE_EVENT, callback);
    }

    getAll() {
        return metrics;
    }
}

var _MetricStore = new MetricStore();
export default _MetricStore;

_MetricStore.dispatchToken = Dispatcher.register((action) => {
//    console.log(action);
    switch (action.type) {
        case ActionTypes.METRIC_STORE_RESET:
            metrics = {};
            _MetricStore.emitChange();
            break;

        case ActionTypes.METRIC_UPDATE:
            metrics[action.name] = action.value;
            _MetricStore.emitChange();
            break;
        default:
    }
});

